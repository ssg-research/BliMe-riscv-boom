package boom.encengine

import chisel3._
import chisel3.util._
import chacha20.ChaCha20Block

class ChaCha20Req extends Bundle {
  val key = Bits(256.W)
  val counter = UInt(32.W) // when is this reset? on every new keystream generation. It gets incremented for every output block
  val nonce = Bits(96.W) // how do we get this?? use the first 12 bytes of input for data import. For data export, write this to first 12 bytes.
}

class ChaCha20Resp extends Bundle {
  val out = Bits(512.W)
}

class ChaCha20(val desired_latency: Int) extends Module {
  val io = IO(new Bundle {
	  val req = Flipped(Decoupled(new ChaCha20Req))
    val resp = Decoupled(new ChaCha20Resp)
	})

  val chacha20block = Module(new ChaCha20Block(desired_latency))
  
  when (io.req.fire) {
    // chacha20block.in.key      := io.req.bits.key
    // chacha20block.in.counter  := io.req.bits.counter
    // chacha20block.in.nonce    := io.req.bits.nonce
    chacha20block.in    := io.req.bits
  } .otherwise {
    chacha20block.in.key        := 0.U
    chacha20block.in.counter    := 0.U
    chacha20block.in.nonce      := 0.U
  }

  val valids = Pipe(true.B, io.req.fire, desired_latency)
  val valid_out = Wire(Decoupled(UInt()))
  valid_out.bits  := chacha20block.out
  valid_out.valid := valids.bits
  val valid_out_q1 = Queue(valid_out, desired_latency, pipe=true, flow=true)
  val valid_out_q2 = Queue(valid_out_q1, 20, pipe=true, flow=true)

   // as long as head buffer has space, receive reqs
   // If for some external reason, q2 is full and stalls, and there are 
   // still valid reqs going through the chacha20block pipeline, we
   // have q1 with desired_latency space to be able to store all the 
   // chacha20block output until the stall is relieved
  io.req.ready := valid_out_q2.ready

  valid_out_q2.ready  := io.resp.ready
  io.resp.valid       := valid_out_q2.valid
  io.resp.bits.out    := valid_out_q2.bits
}