package fpr

class Signal[T](expr: => T) {
  import Signal._

  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set.empty // signals that change when this changes
  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "Cyclic signals!")
    myValue
  }

  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(myExpr()) // make call to self and set expr value
    if (newValue != myValue) {
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }
}

object NoSignal extends Signal[Nothing](???) {
  override protected def computeValue(): Unit = ()
}

object Signal {
  private val caller = new StackableVariable[Signal[_]](NoSignal) // each signal has its own StackableVariable
  def apply[T](expr: => T) = new Signal(expr)
}
