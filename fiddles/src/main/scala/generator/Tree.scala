package generator

import generator.Generator._

trait Tree[T] {
  override def toString: String
}

case class Leaf[T](content: T) extends Tree[T] {
  override def toString: String = s"{'val': '$content'}"
}

case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = s"{'left': $left, 'right': $right}"
}

object Tree {

  def leaf[T](t: T): Generator[Leaf[T]] = single(Leaf(t))

  def node[T](tGen: Generator[T]): Generator[Node[T]] = for {
    t1 <- GeneratorUtils.treeGenerator(tGen)
    t2 <- GeneratorUtils.treeGenerator(tGen)
  } yield Node(t1, t2)
}