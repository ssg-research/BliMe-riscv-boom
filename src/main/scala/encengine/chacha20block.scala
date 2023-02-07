package boom.encengine

package chacha20

import chisel3._
import chisel3.util._

class InvertedEndianness(width: Int, repeat_width: Int, chunk_size: Int) extends Module {
  val repetitions = width / repeat_width

  val chunks = repeat_width / chunk_size
  val in = IO(Input(UInt(width.W)))
  val out = IO(Output(UInt(width.W)))

  val mid = Wire(Vec(width, Bits(1.W)))

  for (repeat <- 0 to repetitions-1) {
    for (i <- 0 to (chunks - 1)) {
      val out_upper = repeat_width*repeat + chunk_size*i + chunk_size-1;
      val out_lower = repeat_width*repeat + chunk_size*i

      val in_upper = repeat_width*repeat + chunk_size*(chunks - i - 1) + chunk_size - 1;
      val in_lower = repeat_width*repeat + chunk_size*(chunks - i - 1);

      for (j <- 0 to chunk_size-1) {
        mid(out_upper - j) := in(in_upper - j)
      }
    }
  }
  out := mid.asUInt()
}

class ChaCha20State extends Bundle {
  val v = Vec(16, UInt(32.W))
}

class ChaCha20QuarterRound extends Module {
  val io = IO(new Bundle {
                val a_pre = Input(UInt(32.W))
                val b_pre = Input(UInt(32.W))
                val c_pre = Input(UInt(32.W))
                val d_pre = Input(UInt(32.W))

                val a_post = Output(UInt(32.W))
                val b_post = Output(UInt(32.W))
                val c_post = Output(UInt(32.W))
                val d_post = Output(UInt(32.W))
              })

  def qround(a: UInt, b: UInt, c: UInt, d: UInt): (UInt, UInt, UInt, UInt) = {
    def fragment(x: UInt, y: UInt, z: UInt, r: Int): (UInt, UInt, UInt) = {
      val x_out = x +% y
      val y_out = y
      val z_intermediate = (z ^ x_out)
      val z_out = Cat(z_intermediate(31-r, 0), z_intermediate(31, 31-r+1))

      return (x_out, y_out, z_out)
    }

    val (a_1, b_1, d_1) = fragment(a, b, d, 16)
    val (c_2, d_2, b_2) = fragment(c, d_1, b_1, 12)
    val (a_3, b_3, d_3) = fragment(a_1, b_2, d_2, 8)
    val (c_4, d_4, b_4) = fragment(c_2, d_3, b_3, 7)


    return (a_3,b_4,c_4,d_4)
  }

  val (a, b, c, d) = qround(io.a_pre, io.b_pre, io.c_pre, io.d_pre)

  io.a_post := a
  io.b_post := b
  io.c_post := c
  io.d_post := d
}

class PerformQuarterRound (a: Int, b: Int, c: Int, d: Int) extends Module {
  val in = IO(Input(new ChaCha20State))
  val out = IO(Output(new ChaCha20State))

  val quarter_round_logic = Module (new ChaCha20QuarterRound);
  quarter_round_logic.io.a_pre := in.v (a);
  quarter_round_logic.io.b_pre := in.v (b);
  quarter_round_logic.io.c_pre := in.v (c);
  quarter_round_logic.io.d_pre := in.v (d);

  out.v (a) := quarter_round_logic.io.a_post;
  out.v (b) := quarter_round_logic.io.b_post;
  out.v (c) := quarter_round_logic.io.c_post;
  out.v (d) := quarter_round_logic.io.d_post;

  for (i <- 0 to 15) {
    if (i != a && i != b && i != c && i != d) {
      out.v(i) := in.v(i)
    }
  }
}

class ChaCha20RoundPair(buffer_first_round_input: Boolean, buffer_second_round_input: Boolean) extends Module {
  val in = IO(Input(new ChaCha20State))
  val out = IO(Output(new ChaCha20State))

  val qr = Vector(
    Module(new PerformQuarterRound(0, 4, 8, 12)),
    Module(new PerformQuarterRound(1, 5, 9, 13)),
    Module(new PerformQuarterRound(2, 6, 10, 14)),
    Module(new PerformQuarterRound(3, 7, 11, 15)),

    Module(new PerformQuarterRound(0, 5, 10, 15)),
    Module(new PerformQuarterRound(1, 6, 11, 12)),
    Module(new PerformQuarterRound(2, 7, 8, 13)),
    Module(new PerformQuarterRound(3, 4, 9, 14)))


  if (buffer_first_round_input) {
	  val input_buffer = RegNext(in);
	  qr(0).in := input_buffer
  }
  else {
	  qr(0).in := in
  }

  for (i <- 1 to 3) {
    qr(i).in := qr(i-1).out
  }

  if (buffer_second_round_input) {
	  val buffer = RegNext(qr(3).out);
	  qr(4).in := buffer
  }
  else {
	  qr(4).in := qr(3).out
  }

  for (i <- 5 to 7) {
    qr(i).in := qr(i-1).out
  }

  out := qr(7).out
}

class ChaCha20Block (desired_latency: Int) extends Module {
  assert(0 <= desired_latency & desired_latency <= 19)

  val rounds_per_stage = 20 / (desired_latency + 1)
  var buffer_positions = Set[Int]()
  for (i <- 1 to desired_latency) {
    buffer_positions += i*rounds_per_stage
  }

  assert(buffer_positions.forall(x => (0 <= x & x <= 20)))



  val in = IO(new Bundle {
                val key = Input(Bits(256.W))
                val counter = Input(UInt(32.W)) // when is this reset? on every new keystream generation. It gets incremented for every output block
                val nonce = Input(Bits(96.W)) // how do we get this?? use the first 12 bytes of input for data import. For data export, write this to first 12 bytes.
              })

  val out = IO(Output(Bits(512.W)))

  val latency = buffer_positions.size


  val key_formatter = Module(new InvertedEndianness(256, 32, 8))
  key_formatter.in := in.key

  val nonce_formatter = Module(new InvertedEndianness(96, 32, 8))
  nonce_formatter.in := in.nonce

  def key_words(i: Int): UInt = key_formatter.out((7-i)*32+31, (7-i)*32)

  def nonce_words(i: Int): UInt = nonce_formatter.out((2-i)*32+31, (2-i)*32)

  val input_state = Wire(new ChaCha20State())
  input_state.v(0) := "h61707865".U
  input_state.v(1) := "h3320646e".U
  input_state.v(2) := "h79622d32".U
  input_state.v(3) := "h6b206574".U
  input_state.v(4) := key_words(0)
  input_state.v(5) := key_words(1)
  input_state.v(6) := key_words(2)
  input_state.v(7) := key_words(3)
  input_state.v(8) := key_words(4)
  input_state.v(9) := key_words(5)
  input_state.v(10) := key_words(6)
  input_state.v(11) := key_words(7)

  input_state.v(12) := in.counter

  input_state.v(13) := nonce_words(0)
  input_state.v(14) := nonce_words(1)
  input_state.v(15) := nonce_words(2)

  var rounds = Seq[ChaCha20RoundPair]()
  for (i <- 0 to 9) {

	  val current_round = 2*i

	  var buffer_0 = false
	  if (buffer_positions.contains(current_round)) {
		  buffer_0 = true
	  }

	  var buffer_1 = false
	  if (buffer_positions.contains(current_round+1)) {
		  buffer_1 = true
	  }

    val round = Module(new ChaCha20RoundPair(buffer_0, buffer_1))
    rounds = rounds :+ round

    if (i == 0) {
      round.in := input_state
    }
    else {
      round.in := rounds(i-1).out
    }
  }


  val rounds_latency = (buffer_positions - 20).size
  val delayed_input_state = ShiftRegister(input_state, rounds_latency)

  val serialized = Wire(Vec(16,UInt(32.W)))
  for (i <- 0 to 15) {
    val reversal = Module(new InvertedEndianness(32, 32, 8));
    reversal.in := rounds(9).out.v(i) +% delayed_input_state.v(i)
    serialized(i) := reversal.out
  }

  val output_state = serialized

  val output_bits = Wire(Bits(512.W))
  output_bits := Cat(output_state(0), output_state(1), output_state(2), output_state(3),
                     output_state(4), output_state(5), output_state(6), output_state(7),
                     output_state(8), output_state(9), output_state(10), output_state(11),
                     output_state(12), output_state(13), output_state(14), output_state(15))
  if (buffer_positions.contains(20)) {
	  val buf = RegNext(output_bits)
	  out := buf
  } else {
	  out := output_bits
  }
}

object ChaCha20Driver extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new ChaCha20Block(19), args)
}
