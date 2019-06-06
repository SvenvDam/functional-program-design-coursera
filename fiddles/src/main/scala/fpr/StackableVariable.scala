package fpr

class StackableVariable[T](init: T) { // T = Signal[_]
  private var values: List[T] = List(init)
  def value: T = values.head // latest Signal calling
  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue :: values // new latest Signal
    try op finally values = values.tail //
  }
}

// val caller = new StackableVariable(initialSig)
// caller.withValue(otherSig) { ... }
//
//