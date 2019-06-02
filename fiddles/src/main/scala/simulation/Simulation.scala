package simulation

trait Simulation {
  import Simulation._

  def currentTime: Int = curTime

  class Wire {

    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal: Boolean = sigVal

    def setSignal(s: Boolean): Unit =
      if (s != sigVal) {
        sigVal = s
        actions.foreach(_ ())
      }

    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val newEvent = Event(curTime + delay, () => block)
    agenda = insert(agenda, newEvent)
  }

  def insert(agenda: Agenda, event: Event): Agenda = agenda match {
    case head :: tail if head.time <= event.time =>
      head :: insert(tail, event)
    case _ =>
      event :: agenda
  }

  def run(): Unit = {
    afterDelay(0)(println("Simulating!!"))
    loop()
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $curTime value = ${wire.getSignal}")
    }
    wire.addAction(probeAction)
  }

  private type Agenda = List[Event]
  private var agenda: Agenda = List.empty
  private var curTime = 0

  private def loop(): Unit = agenda match {
    case head :: tail =>
      agenda = tail
      curTime = head.time
      head.action()
      loop()
    case Nil =>
      ()
  }
}

object Simulation {
  type Action = () => Unit

  case class Event(time: Int, action: Action)
}

