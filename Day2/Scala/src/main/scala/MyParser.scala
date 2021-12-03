import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

import Direction._

class MyParser extends RegexParsers{
  val number: Regex = "[1-9][0-9]*".r
  val direction: Regex = "forward|up|down".r

  def coords: Parser[Movement] = ((direction ^^ {_.toDirection}) ~ (number ^^ {_.toInt})) ^^ {
    a => Movement(a._1, a._2)
  }
  def listCoords: Parser[List[Movement]] = coords.+
}
