package generator

import generator.Tree.{leaf, node}

object GeneratorUtils {
  val integerGenerator: Generator[Int] = new Generator[Int] {
    val rand = new java.util.Random

    def generate = rand.nextInt
  }

  val boolGenerator = for {
    i <- integerGenerator
  } yield i > 0

  def treeGenerator[T](tGen: Generator[T]): Generator[Tree[T]] = for {
    t <- tGen
    b <- boolGenerator
    tree <- if (b) leaf(t) else node(tGen)
  } yield tree
}
