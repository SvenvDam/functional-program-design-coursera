package generator

import GeneratorUtils._

object Main extends App {
  override def main(args: Array[String]): Unit = {
    println(treeGenerator(integerGenerator).generate)
  }
}

