package zio.prelude

/**
 * The error an `Assertion` returns when a value fails it: one failed
 * condition (`Failure`) or several (`Many`).
 */
sealed trait AssertionError { self =>
  import AssertionError._

  /**
   * Combines two errors into one `Many` that lists the failures of this error
   * first and then those of `that`.
   */
  def ++(that: AssertionError): AssertionError =
    (self, that) match {
      case (Many(es1), Many(es2)) => Many((es1 ++ es2).asInstanceOf[::[AssertionError]])
      case (Many(es1), f)         => Many((es1 :+ f).asInstanceOf[::[AssertionError]])
      case (f, Many(es2))         => Many(::(f, es2))
      case (f1, f2)               => Many(::(f1, ::(f2, Nil)))
    }

  /**
   * Returns one message per failed condition for the value written as
   * `value`, such as `"-3 did not satisfy greaterThan(0)"`.
   */
  def toNel(value: String): ::[String] =
    self match {
      case Failure(condition) =>
        ::(s"$value did not satisfy $condition", Nil)
      case Many(head :: tail) =>
        val f :: fs = head.toNel(value)
        ::(f, fs ++ tail.flatMap(_.toNel(value)))
    }

  /**
   * Renders the failures for the value written as `value`, one line per
   * failed condition, each starting with a bullet and colored with ANSI
   * escape codes. The newtype macros use it in compile-time error messages.
   */
  def render(value: String): String = self match {
    case Failure(condition) =>
      scala.Console.RED + "• " + scala.Console.BLUE + value + scala.Console.RESET +
        " did not satisfy " + scala.Console.YELLOW + condition + scala.Console.RESET
    case Many(vector)       =>
      vector.map(_.render(value)).mkString("\n")
  }
}

/** Constructors for `AssertionError`. */
object AssertionError {

  /** Returns an error for one failed condition, described by `condition`. */
  def failure(condition: String): AssertionError = Failure(condition)

  /** One failed condition, such as `between(1000, 9999)`. */
  final case class Failure(condition: String) extends AssertionError

  /** Several failures, in the order they were combined with `++`. */
  final case class Many(vector: ::[AssertionError]) extends AssertionError
}
