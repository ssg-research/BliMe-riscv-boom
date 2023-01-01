package boom.encengine

import chisel3._
import chisel3.util._

class ChaCha20 extends Module {
	val io = IO(new Bundle {
		val req = Flipped(Decoupled(UInt(128.W)))
    	val resp = Decoupled(UInt(128.W))
	})

  val req_in = Queue(io.req, entries = 2, pipe = true, flow = true)
  io.resp <> req_in
  io.resp.bits := Fill(io.resp.bits.getWidth, 1.U)
}