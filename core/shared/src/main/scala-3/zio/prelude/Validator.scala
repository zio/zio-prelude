package zio.prelude

import scala.quoted._



/**
 * A check on values of type `A` that a Scala 3 newtype can run at compile
 * time, for conditions that an [[Assertion]] can't express, such as "the
 * string is a palindrome". The check is a function that returns `Right(())`
 * for a valid value and `Left` with an [[AssertionError]] for an invalid one.
 * `A` needs a `scala.quoted.FromExpr` instance, which Scala provides for the
 * primitive types and `String`.
 *
 * A [[NewtypeCustom]] or [[SubtypeCustom]] uses the validator for both of its
 * checks: `validate` for run-time values and `validateInlineImpl` for the
 * arguments of `apply`. Calling the newtype's `apply` in the same source file
 * as the validator does not compile ("Cannot call macro ... defined in the
 * same source file"), so define the validator in a file of its own:
 *
 * {{{
 * object PalindromeValidator extends Validator[String](str =>
 *   if (str.reverse == str) Right(()) else Left(AssertionError.failure("isPalindrome"))
 * )
 * }}}
 *
 * In another file:
 *
 * {{{
 * // no-check: needs PalindromeValidator from another source file
 * object Palindrome extends SubtypeCustom[String] {
 *   protected def validate(value: String) =
 *     PalindromeValidator.validate(value)
 *
 *   protected inline def validateInline(inline value: String) =
 *     \${ PalindromeValidator.validateInlineImpl('value) }
 * }
 * type Palindrome = Palindrome.Type
 *
 * val ok = Palindrome("racecar")
 * // Palindrome("hello") does not compile: hello did not satisfy isPalindrome
 * val checked = Palindrome.make("hello")
 * // Failure(Chunk(),NonEmptyChunk(hello did not satisfy isPalindrome))
 * }}}
 *
 * @see [[https://zio.dev/zio-prelude/newtypes/ New Types]]
 */
abstract class Validator[A: FromExpr](f: A => Either[AssertionError, Unit]) extends Liftables {
  /**
   * Runs the check on `a`: `Right(())` when it is valid, `Left` with the
   * error otherwise.
   */
  def validate(a: A): Either[AssertionError, Unit] =
    f(a)

  /**
   * The macro implementation to splice in `validateInline`. It reads `expr` as
   * a compile-time constant, runs `validate` on it, and stops compilation
   * with the rendered error when the check fails. An argument that is not a
   * constant, such as a variable, stops compilation with "Expected a known
   * value".
   */
  final def validateInlineImpl(expr: Expr[A])(using Quotes): Expr[Unit] = {
    val a = expr.valueOrAbort
    validate(a) match {
      case Right(_) => '{()}
      case Left(err) =>
        quotes.reflect.report.errorAndAbort(err.render(a.toString))
    }
  }

}
