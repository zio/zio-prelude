package zio

import zio.test.{assert, Assertion, TestResult}
import zio.test.Assertion.{isFalse, isTrue}

package object prelude
  extends DebugSyntax
    with EqualSyntax
    with OrdSyntax
    with HashSyntax
    with ClosureSyntax
    with NewtypeExports
    with StdInstances {

  /**
   * Makes a new assertion that requires a value equal the specified value.
   */
  def equalTo[A: Equal](expected: A): Assertion[A] =
    Assertion.assertion("equalTo")(Assertion.Render.param(expected))(_ === expected)

  /**
   * Provides implicit syntax for asserting that two values of type `A` are
   * equal to each other.
   */
  implicit class AssertSyntax[A](private val self: A) extends AnyVal {
    def <->(that: A)(implicit eq: Equal[A]): TestResult =
      assert(self)(equalTo(that))
  }

  implicit class BoolSyntax(private val l: Boolean) extends AnyVal {
    def or(r: Boolean): TestResult =
      assert(l)(isTrue) || assert(r)(isTrue)

    def ==>(r: Boolean): TestResult =
      assert(r)(isTrue) || assert(l)(isFalse)

    def <==>(r: Boolean): TestResult =
      assert(r)(equalTo(l))
  }
}
