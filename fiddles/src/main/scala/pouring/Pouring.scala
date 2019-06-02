package pouring

class Pouring(capacity: Vector[Int]) {
  type State = Vector[Int]
  val x = capacity
  val initialState: State = capacity.map(_ => 0)

  trait Move {
    def change(state: State): State
  }
  case class Empty(index: Int) extends Move {
    def change(state: State): State = state.updated(index, 0)
  }
  case class Fill(index: Int) extends Move {
    def change(state: State): State = state.updated(index, capacity(index))
  }
  case class Pour(from: Int, to: Int) extends Move {
    def change(state: State): State = {
      val toCap = capacity(to)
      val toFill = state(to)
      val fromFill = state(from)
      val amount = fromFill.min(toCap - toFill)

      state
        .updated(from, fromFill - amount)
        .updated(to, toFill + amount)
    }
  }

  val glasses = capacity.indices.toSet

  val moves: Set[Move] = {
    (for (g <- glasses) yield Empty(g)) ++
      (for (g <- glasses) yield Fill(g)) ++
      (for {
        g1 <- glasses
        g2 <- glasses
        if g1 != g2
      } yield Pour(g1, g2))
  }

  class Path(history: List[Move]) {
    lazy val endState: State = history.foldRight(initialState) {
      case (move, s) => move.change(s)
    }

    def extend(move: Move): Path = new Path(move :: history)

    override def toString = history.reverse.mkString(" ") + " ---> " + endState
  }

  val initialPath = new Path(Nil)

  def generate(paths: Set[Path], explored: Set[State] = Set.empty): Stream[Set[Path]] = {
    if (paths.isEmpty) Stream.empty
    else {
      val next = for {
        path <- paths
        n <- moves.map(path.extend)
        if !explored.contains(n.endState)
      } yield n
      paths #:: generate(next, explored ++ next.map(_.endState))
    }
  }

  def solve(target: Int): Path = {
    val candidates = for {
      pathSet <- generate(Set(initialPath))
      path <- pathSet
      if path.endState.contains(target)
    } yield path
    candidates.head
  }
}
