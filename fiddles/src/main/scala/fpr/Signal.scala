package fpr

class Signal[T](expr: => T) {
  def apply(): T = ???

}

object Signal {
  def apply[T](expr: => T) = new Signal(expr)
}
