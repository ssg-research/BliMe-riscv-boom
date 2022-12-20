package boom.encengine

import chisel3._
import chisel3.util._
import chisel3.experimental.{requireIsChiselType, DataMirror, Direction}
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.config._
import freechips.rocketchip.rocket._

class MemBufElement(implicit val p: Parameters) extends Bundle 
  with HasCoreParameters 
  with HasCoreData {
  // val addr        = UInt(coreMaxAddrBits.W)
  val tag         = UInt(coreParams.dcacheReqTagBits.W)
  val data_valid  = Bool()
}

class MemBuf(
//   val gen:            T,
  val entries:        Int,
  val pipe:           Boolean = false,
  val flow:           Boolean = false,
  val hasFlush:       Boolean = false
)(implicit p: Parameters)//,
  // implicit compileOptions: chisel3.CompileOptions)
    extends Module() {
  require(entries > 0, "MemBuf must have positive number of entries")
  val gen = new MemBufElement
  // val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    // requireIsChiselType(gen)
    // gen
  // } else {
    // if (DataMirror.internal.isSynthesizable(gen)) {
      // chiselTypeOf(gen)
    // } else {
      // gen
    // }
  // }
  val genType = gen

  val io = IO(new Bundle {
    val q = new QueueIO(genType, entries, hasFlush)
    // val all_addrs_out = Output(Vec(entries, Valid(genType.addr)))
    val all_tags_out = Output(Vec(entries, Valid(genType.tag)))
    val side_index_in  = Input(UInt((log2Ceil(entries).W)))
    val side_data_in   = Input(genType.data)
    val side_data_valid_in = Input(genType.data_valid)
  })
  // val ram = if (useSyncReadMem) SyncReadMem(entries, genType, SyncReadMem.WriteFirst) else Mem(entries, genType)
  val buf = Reg(Vec(entries, genType))
  val valids = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = RegInit(false.B)
  val ptr_match = enq_ptr.value === deq_ptr.value
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  val do_enq = WireDefault(io.q.enq.fire)
  val do_deq = WireDefault(io.q.deq.fire)
  val flush = io.q.flush.getOrElse(false.B)

  // when flush is high, empty the queue
  // Semantically, any enqueues happen before the flush.
  when(do_enq) {
    buf(enq_ptr.value) := io.q.enq.bits
    valids(enq_ptr.value) := true.B
    enq_ptr.inc()
  }
  when(do_deq) {
    valids(deq_ptr.value) := false.B
    deq_ptr.inc()
  }
  when(do_enq =/= do_deq) {
    maybe_full := do_enq
  }
  when(flush) {
    enq_ptr.reset()
    deq_ptr.reset()
    maybe_full := false.B
  }

  io.q.deq.valid := !empty
  io.q.enq.ready := !full
  io.all_tags_out.zipWithIndex.foreach{case (out, i) => 
    // out.bits := buf(i).addr
    out.bits := buf(i).tag
    // valid is a Bool that depends on values of enq_ptr and deq_ptr; use Scala magic OR
    // just create and use a valids Reg vector :)
    out.valid := valids(i)
  }
  when(io.side_data_valid_in) {
    buf(io.side_index_in).data_valid := true.B
    buf(io.side_index_in).data := io.side_data_in
  }

  // if (useSyncReadMem) {
  //   val deq_ptr_next = Mux(deq_ptr.value === (entries.U - 1.U), 0.U, deq_ptr.value + 1.U)
  //   val r_addr = WireDefault(Mux(do_deq, deq_ptr_next, deq_ptr.value))
  //   io.q.deq.bits := buf.read(r_addr)
  // } else {
    io.q.deq.bits := buf(deq_ptr.value)
  // }

  if (flow) {
    when(io.q.enq.valid) { io.q.deq.valid := true.B }
    when(empty) {
      io.q.deq.bits := io.q.enq.bits
      do_deq := false.B
      when(io.q.deq.ready) { do_enq := false.B }
    }
  }

  if (pipe) {
    when(io.q.deq.ready) { io.q.enq.ready := true.B }
  }

  val ptr_diff = enq_ptr.value - deq_ptr.value

  if (isPow2(entries)) {
    io.q.count := Mux(maybe_full && ptr_match, entries.U, 0.U) | ptr_diff
  } else {
    io.q.count := Mux(
      ptr_match,
      Mux(maybe_full, entries.asUInt, 0.U),
      Mux(deq_ptr.value > enq_ptr.value, entries.asUInt + ptr_diff, ptr_diff)
    )
  }
}