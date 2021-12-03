import Solution.readStr
import cats.effect.{ExitCode, IO, IOApp}

import scala.annotation.tailrec
import scala.io.BufferedSource

object Solution {
  def readStr(path: String = "file.txt"): List[Int] = {
    val source: BufferedSource = scala.io.Source.fromFile(path)
    try source.mkString.split("\r\n").toList.map(_.toInt) finally source.close()
  }
}

object MainApp extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    @tailrec
    def calculateScore(list: List[Int], acc: Int): Int = {
      list match {
        case head :: next :: tail => if (head < next) calculateScore(next :: tail, acc + 1) else calculateScore(next :: tail, acc)
        case _ => acc
      }
    }
    val list = readStr("dataset.txt")
    print(calculateScore(list, 0))
    IO(ExitCode.Success)
  }
}

object Part2 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    @tailrec
    def calculateScore(list: List[Int], acc: Int): Int = {
      list match {
        case head :: next :: tail => if (head < next) calculateScore(next :: tail, acc + 1) else calculateScore(next :: tail, acc)
        case _ => acc
      }
    }
    val list = readStr("dataset.txt")
    val newList = list.sliding(3, 1).map(_.sum).toList
    print(calculateScore(newList, 0))
    IO(ExitCode.Success)
  }
}
