package zio.prelude

import zio.NonEmptyChunk

sealed trait RefinementError { self =>
  import RefinementError._

  def ++(that: RefinementError): RefinementError =
    (self, that) match {
      case (Many(es1), Many(es2)) => Many(es1 ++ es2)
      case (Many(es1), f)         => Many(es1 :+ f)
      case (f, Many(es2))         => Many(f +: es2)
      case (f1, f2)               => Many(NonEmptyChunk(f1, f2))
    }

  def toNonEmptyChunk(value: String): NonEmptyChunk[String] =
    self match {
      case Failure(condition) =>
        NonEmptyChunk(s"$value did not satisfy $condition")
      case Many(vector)       =>
        vector.flatMap(_.toNonEmptyChunk(value))
    }

  def render(value: String): String = self match {
    case Failure(condition) =>
      scala.Console.RED + "• " + scala.Console.BLUE + value + scala.Console.RESET +
        " did not satisfy " + scala.Console.YELLOW + condition + scala.Console.RESET
    case Many(vector)       =>
      vector.map(_.render(value)).mkString("\n")
  }
}

object RefinementError {
  def failure(condition: String): RefinementError = Failure(condition)

  final case class Failure(condition: String)                   extends RefinementError
  final case class Many(vector: NonEmptyChunk[RefinementError]) extends RefinementError
}
