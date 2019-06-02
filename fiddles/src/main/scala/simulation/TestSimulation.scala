package simulation

object TestSimulation extends App with Circuits {
  val in1, in2, sum, carry = new Wire

  halfAdder(in1,in2, sum, carry)
  probe("sum", sum)
  probe("carry", carry)

  in1.setSignal(true)
  in2.setSignal(true)
  run()
}
