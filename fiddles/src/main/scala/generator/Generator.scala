package generator

trait Generator[+T] {

  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    def generate: S = f(Generator.this.generate)
  }

  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    def generate: S = f(Generator.this.generate).generate
  }
}

object Generator {

  def single[T](t: T): Generator[T] = new Generator[T] {
    def generate: T = t
  }
}