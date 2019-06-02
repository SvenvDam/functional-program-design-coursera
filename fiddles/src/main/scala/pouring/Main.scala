package pouring

object Main extends App {
  override def main(args: Array[String]): Unit = {
    val pouring = new Pouring(Vector[Int](4, 9, 3))
    val solution = pouring.solve(5)
    println(solution)
  }
}
