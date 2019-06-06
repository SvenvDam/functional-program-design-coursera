package calculator

object Polynomial {
  def computeDelta(a: Signal[Double],
                   b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double],
                       b: Signal[Double],
                       c: Signal[Double],
                       delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(computeDelta(a, b, c)() match {
      case discr if discr < 0  => Set.empty
      case discr if discr == 0 => Set(-b())
      case discr               => Set(-b() + math.sqrt(discr), -b() - math.sqrt(discr))
    })
  }
}
