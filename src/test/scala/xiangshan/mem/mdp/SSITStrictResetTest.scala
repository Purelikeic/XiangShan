package xiangshan.mem.mdp

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan._

class SSITStrictResetHarness(implicit p: org.chipsalliance.cde.config.Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ren = Input(Vec(DecodeWidth, Bool()))
    val raddr = Input(Vec(DecodeWidth, UInt(MemPredPCWidth.W)))
    val rdata = Output(Vec(RenameWidth, new SSITEntry))
    val update = Input(new MemPredUpdateReq)
  })

  val ssit = Module(new SSIT)
  ssit.io.ren := io.ren
  ssit.io.raddr := io.raddr
  io.rdata := ssit.io.rdata
  ssit.io.update := io.update
  ssit.io.csrCtrl := 0.U.asTypeOf(chiselTypeOf(ssit.io.csrCtrl))
  ssit.io.csrCtrl.lvpred_timeout := 31.U
}

class SSITStrictResetTest extends AnyFlatSpec with Matchers with ChiselSim {
  private val defaultConfig = new top.DefaultConfig
  implicit private val config: org.chipsalliance.cde.config.Parameters = defaultConfig.alterPartial {
    case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy()
  }.alter((site, here, up) => {
    case LogUtilsOptionsKey => LogUtilsOptions(
      enableDebug = here(DebugOptionsKey).EnableDebug,
      enablePerf = here(DebugOptionsKey).EnablePerfDebug,
      fpgaPlatform = here(DebugOptionsKey).FPGAPlatform,
      enableXMR = here(DebugOptionsKey).EnableXMR
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false,
      enablePerfDB = false,
      perfLevel = XSPerfLevel.NORMAL,
      perfDBHartID = 0
    )
  })

  private case class Entry(valid: Boolean, ssid: BigInt, strict: Boolean)

  behavior of "SSIT strict reset"

  it should "expire old strict entries while preserving retraining during scrub" in {
    import chisel3.simulator.HasSimulator.simulators.verilator

    simulate(new SSITStrictResetHarness) { dut =>
      dut.io.ren.foreach(_.poke(false.B))
      dut.io.raddr.foreach(_.poke(0.U))
      dut.io.update.valid.poke(false.B)
      dut.io.update.waddr.poke(0.U)
      dut.io.update.wdata.poke(false.B)
      dut.io.update.ldpc.poke(0.U)
      dut.io.update.stpc.poke(0.U)

      dut.clock.step(1030)

      def update(ldpc: Int, stpc: Int): Unit = {
        dut.io.update.ldpc.poke(ldpc.U)
        dut.io.update.stpc.poke(stpc.U)
        dut.io.update.valid.poke(true.B)
        dut.clock.step()
        dut.io.update.valid.poke(false.B)
        dut.clock.step(5)
      }

      def makeStrict(ldpc: Int, stpc: Int): Unit = {
        update(ldpc, stpc)
        update(ldpc, stpc)
      }

      def read(addr: Int): Entry = {
        dut.io.raddr(0).poke(addr.U)
        dut.io.ren(0).poke(true.B)
        dut.clock.step()
        val entry = Entry(
          dut.io.rdata(0).valid.peek().litToBoolean,
          dut.io.rdata(0).ssid.peek().litValue,
          dut.io.rdata(0).strict.peek().litToBoolean
        )
        dut.io.ren(0).poke(false.B)
        entry
      }

      def waitUntilStrictClears(addr: Int, maxCycles: Int): Int = {
        dut.io.raddr(0).poke(addr.U)
        dut.io.ren(0).poke(true.B)
        dut.clock.step()
        var cycles = 0
        while (dut.io.rdata(0).strict.peek().litToBoolean && cycles <= maxCycles) {
          dut.clock.step()
          cycles += 1
        }
        dut.io.ren(0).poke(false.B)
        cycles
      }

      val (oldLd, oldSt) = (800, 801)
      val (retrainedLd, retrainedSt) = (900, 901)

      makeStrict(oldLd, oldSt)
      val oldBefore = read(oldLd)
      oldBefore.valid mustBe true
      oldBefore.strict mustBe true

      dut.clock.step(1024)

      makeStrict(retrainedLd, retrainedSt)
      val retrainedBefore = read(retrainedLd)
      retrainedBefore.valid mustBe true
      retrainedBefore.strict mustBe true

      val firstClearWait = waitUntilStrictClears(oldLd, 8192)
      firstClearWait must be > 6800
      firstClearWait must be < 7500

      val oldAfterClear = read(oldLd)
      oldAfterClear.valid mustBe oldBefore.valid
      oldAfterClear.ssid mustBe oldBefore.ssid
      oldAfterClear.strict mustBe false

      val retrainedAfterClear = read(retrainedLd)
      retrainedAfterClear.valid mustBe retrainedBefore.valid
      retrainedAfterClear.ssid mustBe retrainedBefore.ssid
      retrainedAfterClear.strict mustBe false

      update(retrainedLd, retrainedSt)
      read(retrainedLd).strict mustBe true

      dut.clock.step(1030)
      read(oldLd).strict mustBe false
      read(retrainedLd).strict mustBe true

      waitUntilStrictClears(retrainedLd, 8192) must be <= 8192
      read(oldLd).strict mustBe false
      read(retrainedLd).strict mustBe false
    }
  }
}
