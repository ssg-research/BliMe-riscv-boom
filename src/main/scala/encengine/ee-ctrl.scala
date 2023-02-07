package boom.encengine

import chisel3._
import chisel3.util._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import Chisel.ImplicitConversions._
import scala.collection.mutable.HashMap
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util.{BlindedMem,Blinded}

class EECtrlModule()(implicit val p: Parameters) extends Module
  with HasCoreParameters
  with MemoryOpConstants {

  val io = IO(new Bundle {
    val rocc_req_val      = Input(Bool())
    val rocc_req_rdy      = Output(Bool())
    val rocc_funct        = Input(Bits(2.W))
    val rocc_rs1          = Input(Blinded(Bits(64.W)))
    val rocc_rs2          = Input(Blinded(Bits(64.W)))
    val rocc_rd           = Input(Bits(5.W))

    val busy              = Output(Bool())

    val dmem_req_val      = Output(Bool())
    val dmem_req_rdy      = Input(Bool())
    val dmem_req_tag      = Output(Bits(coreParams.dcacheReqTagBits.W))
    val dmem_req_addr     = Output(Bits(coreMaxAddrBits.W))
    val dmem_req_cmd      = Output(Bits(M_SZ.W))
    val dmem_req_size     = Output(Bits(log2Ceil(coreDataBytes + 1).W))
    val dmem_req_data     = Output(BlindedMem(Bits(coreDataBits.W), Bits(coreDataBytes.W)))

    val dmem_resp_val     = Input(Bool())
    val dmem_resp_tag     = Input(Bits(7.W))
    val dmem_resp_data    = Input(BlindedMem(Bits(coreDataBits.W), Bits(coreDataBytes.W)))

    val sfence            = Output(Bool())

    val session_key       = Input(UInt())
    val init              = Output(Bool())

    // val buffer_out  = Output(Bits(width.W))
  })

  // ###### things to move out of controller #######
  // ks_buf length = 16, ks_buf width = 64
  val ks_buf = Module(new Queue(UInt(64.W), 16))

  // mem_buf length = 16, mem_buf width = 64
  val mem_buf = Module(new MemBuf(16, false, false, false))

  val chacha = Module(new ChaCha20(10))

  // ###############################################

  val start_addr = Mux(io.rocc_rs1.blinded, 0.U, io.rocc_rs1.bits)
  val length     = Mux(io.rocc_rs2.blinded, 0.U, io.rocc_rs2.bits) // NB: We assume length is always a multiple of 8

  // val busy = RegInit(false.B)

  // mem read req state machine signals
  val mrq_init :: mrq_send :: mrq_done :: Nil = Enum(3)
  val mrq_s = RegInit(mrq_init)
  val current_read_addr = RegInit(0.U(start_addr.getWidth.W))
  val read_tag = RegInit(0.U((io.dmem_req_tag.getWidth-1).W))
  val block_read = Wire(Bool())
  val last_block_read = RegNext(block_read)

  // xor state machine signals
  // val x_init :: x_write :: Nil = Enum(2) // this is redundant
  // val x_s = RegInit(x_init)
  val do_write = Wire(Bool())
  val last_do_write = RegNext(do_write)
  val current_write_addr = RegInit(0.U(start_addr.getWidth.W))
  val write_tag = RegInit(0.U((io.dmem_req_tag.getWidth-1).W))

  // keystream req state machine signals
  // val ksq_init :: ksq_gen :: Nil = Enum(2)
  // val ksq_s = RegInit(ksq_init)
  val session_key_width = p(EESessionKeyWidthP)
  val session_key = Reg(Valid(UInt(session_key_width.W)))
  // session_key.bits := 0.U
  session_key.bits := "h000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f".U
  session_key.valid := true.B
  // val nonce = RegInit(Valid(UInt(128.W)), )
  val nonce = RegInit(Valid(UInt(128.W)), {
    val bundle = Wire(Valid(UInt(128.W)))
    bundle.bits  := 0.U
    bundle.valid := false.B
    bundle
  })
  // val counter = new Counter(scala.math.pow(2, 33).toInt)
  val counter_reset = WireDefault(false.B)
  val counter =  withReset(counter_reset)(new Counter(scala.math.pow(2, 33).toInt))

  val cmd_complete = RegInit(false.B)
  val busy = RegInit(false.B)

  // default
  io.busy := busy
  io.rocc_req_rdy := false.B
  io.init := false.B
  io.dmem_req_val:= false.B
  io.dmem_req_tag:= 0.U
  io.dmem_req_addr:= 0.U
  io.dmem_req_cmd:= M_XRD
  io.dmem_req_size:= log2Ceil(8).U
  io.dmem_req_data := DontCare
  io.sfence      := false.B
  mem_buf.io.side_data_valid_in := false.B
  mem_buf.io.side_index_in := DontCare
  mem_buf.io.side_data_in := DontCare
  mem_buf.io.q.enq.valid := false.B
  mem_buf.io.q.enq.bits := DontCare
  mem_buf.io.q.deq.ready := false.B
  ks_buf.io.deq.ready := false.B


  // read and write ctrl signals
  do_write := mem_buf.io.q.deq.valid && mem_buf.io.q.deq.bits.data_valid && ks_buf.io.deq.valid
  block_read := do_write || !mem_buf.io.q.enq.ready

  // keystream gen connections
  ks_buf.io.enq.bits    := chacha.io.resp.bits.out
  ks_buf.io.enq.valid   := chacha.io.resp.valid
  chacha.io.resp.ready  := ks_buf.io.enq.ready
  chacha.io.req.valid         := session_key.valid && nonce.valid
  chacha.io.req.bits.key      := session_key.bits
  chacha.io.req.bits.counter  := counter.value
  chacha.io.req.bits.nonce    := nonce.bits
  when (chacha.io.req.fire) {
    counter.inc()
  }

  def try_send_readreq(read_addr : UInt) = {
    when (!block_read) {
      // here, we assume the read req is sent
      io.dmem_req_val   := true.B
      io.dmem_req_addr  := read_addr
      io.dmem_req_tag   := Cat(0.U(1.W), read_tag)
      io.dmem_req_cmd   := M_XRD

      when (io.dmem_req_rdy) {
        current_read_addr                 := read_addr + 8
        read_tag                          := read_tag + 1
        
        when (read_addr >= (start_addr + 16.U)) {
          // save a slot in the membuf
          mem_buf.io.q.enq.valid            := true.B
          // mem_buf.io.q.enq.bits.addr        := read_addr
          mem_buf.io.q.enq.bits.tag         := read_tag
          mem_buf.io.q.enq.bits.data_valid  := false.B

          when (read_addr === (start_addr + length - 8)) { // last read req
            mrq_s := mrq_done
          }
        }
      }
    }
  }

  def send_writereq(write_addr : UInt) = {
    io.dmem_req_val     := true.B
    io.dmem_req_addr    := write_addr //start_addr + (mem_buf.io.q.deq.bits.tag * 8)
    io.dmem_req_tag     := Cat(1.U(1.W), write_tag)
    io.dmem_req_cmd     := M_XWR

    when (write_addr >= (start_addr + 16.U)) {
      io.dmem_req_data.bits       := mem_buf.io.q.deq.bits.data.bits ^ ks_buf.io.deq.bits
      assert(mem_buf.io.q.deq.bits.data.blindmask.andR === mem_buf.io.q.deq.bits.data.blindmask.orR, "Assumption that blindmask bits are all the same is not true!")
      io.dmem_req_data.blindmask  := Mux(io.rocc_funct === 0.U, true.B, false.B)
    } .otherwise {
      io.dmem_req_data.bits       := Mux(write_addr === start_addr, nonce.bits(63,0), Cat(Fill(32, 0.U), nonce.bits(95,64)))
      io.dmem_req_data.blindmask  := false.B
    }

    when (io.dmem_req_rdy) {
      current_write_addr      := write_addr + 8 //start_addr + mem_buf.io.q.deq.bits.tag * 8
      write_tag               := write_tag + 1
      // when (write_tag === Fill(io.dmem_req_tag.getWidth, 1.U)) {
      //   write_tag := 1.U
      // }
      when (write_addr >= (start_addr + 16.U)) {
        mem_buf.io.q.deq.ready  := true.B // dequeue top membuf element
        ks_buf.io.deq.ready     := true.B // dequeue top ksbuf element

        assert(current_write_addr <= start_addr + length)

        when (current_write_addr === (start_addr + length - 8)) { // last write req. we assume addresses in membuf are in-order
          cmd_complete := true.B
        }
      }
    }
  }

  // mem read req state machine processing
  switch(mrq_s) {
  is(mrq_init) {
    io.init := true.B
    io.rocc_req_rdy := true.B
    busy := false.B
    current_read_addr := 0.U
    current_write_addr := 0.U
    read_tag := 0.U
    write_tag := 0.U
    nonce.bits := 0.U
    nonce.valid := false.B
    counter_reset := true.B
    when (io.rocc_req_val && length > 16.U && (io.rocc_funct === 0.U || io.rocc_funct === 1.U)) {
      mrq_s := mrq_send
      try_send_readreq(Mux(io.rocc_funct === 0.U, start_addr, start_addr + 16.U))
      current_write_addr := Mux(io.rocc_funct === 0.U, start_addr + 16.U, start_addr)
      when (io.rocc_funct === 1.U) {
        nonce.valid := true.B
        nonce.bits := chisel3.util.random.LFSR(96)
      }
      io.rocc_req_rdy := false.B
      busy := true.B
      counter_reset := false.B
    }
  }
  is(mrq_send) {
    try_send_readreq(current_read_addr)
  }
  is(mrq_done) {
    when (cmd_complete) {
      mrq_s := mrq_init
      cmd_complete := false.B
      io.rocc_req_rdy := true.B
      counter_reset := true.B
    }
  }
  }

  // xor and write
  when (io.rocc_req_val && do_write) {
    send_writereq(current_write_addr)
  }

  // mem resp handling: reads and writes
  when (io.dmem_resp_val) {
    when (io.dmem_resp_tag(io.dmem_req_tag.getWidth-1) === 0.U) {
      // read response
      val read_resp_tag = io.dmem_resp_tag(io.dmem_req_tag.getWidth-2,0)
      when (io.rocc_funct === 0.U && read_resp_tag < 2.U) {
        when (read_resp_tag === 0.U) {
          nonce.bits := Cat(0.U(64.W), Mux(io.dmem_resp_data.blindmask.orR, 0.U(64.W), io.dmem_resp_data.bits(63,0)))
          nonce.valid      := false.B
        } .otherwise {
          nonce.bits := Cat(0.U(32.W), Mux(io.dmem_resp_data.blindmask.orR, 0.U(32.W), io.dmem_resp_data.bits(31,0)), nonce.bits(63,0))
          nonce.valid := true.B
          assert(nonce.bits(127,96) === 0.U)
        }
      } .otherwise {
        val membuf_idx = Wire(Valid(UInt()))
        membuf_idx.valid := false.B
        membuf_idx.bits := 0.U
        // membuf_idx := MuxLookup(read_resp_tag, 0.U, mem_buf.io.all_addrs_out.zipWithIndex.map{case (addr, i) => 
        mem_buf.io.all_tags_out.zipWithIndex.foreach{case (tag, i) => 
          when (tag.valid && tag.bits === read_resp_tag) {
            membuf_idx.valid := true.B
            membuf_idx.bits := i.U
          }
        }
        // assert(membuf_idx.valid, "[ee-ctrl] mem resp does not have a corresponding slot in membuf!")
        mem_buf.io.side_index_in := membuf_idx.bits
        mem_buf.io.side_data_in := io.dmem_resp_data
        mem_buf.io.side_data_valid_in := membuf_idx.valid
      }

    }
    // otherwise (write resp), do nothing
  }
}