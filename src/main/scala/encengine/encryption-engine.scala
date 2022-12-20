package boom.encengine

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util.BlindedMem


case object EESessionKeyWidthP extends Field[Int]
case object EEFastMem extends Field[Boolean]
case object EEBufferSram extends Field[Boolean]

class EncryptionEngineRoCC(opcodes: OpcodeSet)
    (implicit p: Parameters) extends LazyRoCC(opcodes, nPTWPorts = if (p(EncEngineTLB).isDefined) 1 else 0) {
  override lazy val module = new EncryptionEngineRoCCModule(this)
  val dmemOpt = p(EncEngineTLB).map { _ =>
    val dmem = LazyModule(new DmemModule)
    tlNode := dmem.node
    dmem
  }
}

class EncryptionEngineRoCCModule(outer: EncryptionEngineRoCC)
    extends LazyRoCCModuleImp(outer) {
  val cmd = Queue(io.cmd, 1, true, false)
  // The parts of the command are as follows
  // inst - the parts of the instruction itself
  //   opcode
  //   rd - destination register number
  //   rs1 - first source register number
  //   rs2 - second source register number
  //   funct
  //   xd - is the destination register being used?
  //   xs1 - is the first source register being used?
  //   xs2 - is the second source register being used?
  // rs1 - the value of source register 1
  // rs2 - the value of source register 2

  val SESSION_KEY_WIDTH = 128
  

  val ctrl = Module(new EECtrlModule()(p))
  // val chacha = Module(new ChaCha20Block)
  val session_key = RegInit(UInt(p(EESessionKeyWidthP).W), "h0000".U)
  
  ctrl.io.rocc_req_val    := cmd.valid
  cmd.ready               := ctrl.io.rocc_req_rdy
  ctrl.io.rocc_funct      := cmd.bits.inst.funct
  ctrl.io.rocc_rs1        := cmd.bits.rs1
  ctrl.io.rocc_rs2        := cmd.bits.rs2
  ctrl.io.rocc_rd         := cmd.bits.inst.rd
  io.busy                 := ctrl.io.busy
  ctrl.io.session_key     := session_key

  // val status = RegEnable(io.cmd.bits.status, io.cmd.fire())
  val status = cmd.bits.status
  // val dmem_data = Wire(BlindedMem(Bits(), Bits()))
  def dmem_ctrl(req: DecoupledIO[HellaCacheReq]) = {
    req.valid := ctrl.io.dmem_req_val
    ctrl.io.dmem_req_rdy := req.ready
    req.bits.tag := ctrl.io.dmem_req_tag
    req.bits.addr := ctrl.io.dmem_req_addr
    req.bits.cmd := ctrl.io.dmem_req_cmd
    req.bits.size := ctrl.io.dmem_req_size
    // req.bits.data := dmem_data
    req.bits.data := ctrl.io.dmem_req_data
    req.bits.signed := false.B
    req.bits.dprv := status.dprv
    req.bits.dv := status.dv
    req.bits.phys := false.B
  }

  outer.dmemOpt match {
    case Some(m) => {
      val dmem = m.module
      dmem_ctrl(dmem.io.req)
      io.mem.req <> dmem.io.mem
      io.ptw.head <> dmem.io.ptw

      dmem.io.status := status
      dmem.io.sfence := ctrl.io.sfence
    }
    case None => dmem_ctrl(io.mem.req)
  }

  // dmem_data := ?

  ctrl.io.dmem_resp_val  <> io.mem.resp.valid
  ctrl.io.dmem_resp_tag  <> io.mem.resp.bits.tag
  ctrl.io.dmem_resp_data := io.mem.resp.bits.data

  // chacha20.io.ks_req.bits.key      := session_key
  // chacha20.io.ks_req.bits.nonce    := 0.U
  // chacha20.io.ks_req.bits.counter  := 0.U
  // chacha20.io.ks_req.valid         := (smthng from ctrl)
  // ? (smthng in ctrl)               := chacha20.io.ks_resp.valid

  

  // TODO connect ChaChaBlock here
}

class WithEncEngine extends Config ((site, here, up) => {
  case EESessionKeyWidthP => 128
  case EEFastMem => true
  case EEBufferSram => false
  case EncEngineTLB => Some(TLBConfig(nSets = 1, nWays = 4, nSectors = 1, nSuperpageEntries = 1))
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      val encEng = LazyModule.apply(new EncryptionEngineRoCC(OpcodeSet.custom0)(p))
      encEng
    }
  )
})