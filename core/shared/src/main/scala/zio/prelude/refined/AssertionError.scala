package zio.prelude.refined

sealed trait AssertionError { self =>
  import AssertionError._

  def ++(that: AssertionError): AssertionError =
    (self, that) match {
      case (Many(es1), Many(es2)) => Many(es1 ++ es2)
      case (Many(es1), f)         => Many(es1 :+ f)
      case (f, Many(es2))         => Many(f +: es2)
      case (f1, f2)               => Many(Vector(f1, f2))
    }

  def render: String = self match {
    case Failure(condition, value) =>
      scala.Console.RED + "• " + scala.Console.BLUE + value + scala.Console.RESET +
        " did not satisfy " + scala.Console.YELLOW +
        condition + scala.Console.RESET
    case Many(vector)              =>
      vector.mkString("\n")
  }
}

object AssertionError {
  case class Failure(condition: String, value: String) extends AssertionError
  case class Many(vector: Vector[AssertionError])      extends AssertionError
}
