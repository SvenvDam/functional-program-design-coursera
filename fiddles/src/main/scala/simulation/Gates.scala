package simulation

trait Gates extends Simulation {
  val inverterDelay = 2
  val andDelay = 3
  val orDelay = 5

  def inverter(in: Wire, out: Wire): Unit = {
    def invertAction(): Unit = {
      val inSig = in.getSignal
      afterDelay(inverterDelay) {
        out.setSignal(!inSig)
      }
    }

    in.addAction(invertAction)
  }

  def and(in1: Wire, in2: Wire, out: Wire): Unit = {
    def andAction(): Unit = {
      val inSig1 = in1.getSignal
      val inSig2 = in2.getSignal
      afterDelay(andDelay) {
        out.setSignal(inSig1 & inSig2)
      }
    }

    in1.addAction(andAction)
    in2.addAction(andAction)
  }

  def or(in1: Wire, in2: Wire, out: Wire): Unit = {
    def orAction(): Unit = {
      val inSig1 = in1.getSignal
      val inSig2 = in2.getSignal
      afterDelay(orDelay) {
        out.setSignal(inSig1 | inSig2)
      }
    }

    in1.addAction(orAction)
    in2.addAction(orAction)
  }
}
