package simulation

abstract class BasicSimulation extends Simulation {

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {

    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal: Boolean = sigVal

    def setSignal(s: Boolean) =
      if (s != sigVal) {
        sigVal = s
        actions.foreach(_())
      }

    def addAction(a: Action) = {
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire) = {
    def inverterAction() = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal !inputSig
      }
    }
    input addAction inverterAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire): Unit = {
    def andActon() = {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) {
        output setSignal (a1Sig & a2Sig)
      }
    }
    a1 addAction andActon
    a2 addAction andActon
  }

  def orGate(a1: Wire, a2: Wire, output: Wire): Unit = {
    def orActon() = {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) {
        output setSignal (a1Sig | a2Sig)
      }
    }
    a1 addAction orActon
    a2 addAction orActon
  }

  def probe(name: String, wire: Wire) = {
    def probeAction() = {
      println(s"$name $currentTime new value ${wire.getSignal}")
    }
    wire addAction probeAction
  }

}
