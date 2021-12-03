import cats.effect.{ExitCode, IO, IOApp}

trait Direction

object Direction {
  case object Up extends Direction
  case object Down extends Direction
  case object Forward extends Direction

  implicit class toDirection(val string: String) {
    def toDirection: Direction = string match {
      case a if a == "up" => Direction.Up
      case a if a == "down" => Direction.Down
      case a if a == "forward" => Direction.Forward
      case _ => new Direction {}
    }
  }
}

case class Movement(direction: Direction, int: Int)

case class Submarine(vertical: Int, horizontal: Int) {
  override def toString: String = (vertical * horizontal).toString

  def move(movement: Movement): Submarine = {
    val direction = movement.direction
    val value = movement.int
    direction match {
      case Direction.Down => Submarine(vertical + value, horizontal)
      case Direction.Forward => Submarine(vertical, horizontal + value)
      case Direction.Up => Submarine(vertical - value, horizontal)
      case _ => Submarine(vertical, horizontal)
    }
  }
}

case class NewSubmarine(aim: Int, override val vertical: Int, override val horizontal: Int) extends Submarine(vertical, horizontal) {
  override def move(movement: Movement): NewSubmarine = {
    val direction = movement.direction
    val value = movement.int
    direction match {
      case Direction.Down => NewSubmarine(aim + value, vertical, horizontal)
      case Direction.Up => NewSubmarine(aim - value, vertical, horizontal)
      case Direction.Forward => NewSubmarine(aim, vertical + (value * aim), horizontal + value)
      case _ => NewSubmarine(aim, vertical, horizontal)
    }
  }
}

object Part1 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val source = scala.io.Source.fromFile("dataset.txt")
    val line = try source.mkString finally source.close()
    val parser = new MyParser
    val parsed = parser.parseAll(parser.listCoords, line).get
    print(parsed.foldLeft(NewSubmarine(0, 0, 0))((sub, mov) => sub.move(mov)))
    IO(ExitCode.Success)
  }
}
