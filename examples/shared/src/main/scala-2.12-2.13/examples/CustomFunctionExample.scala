package examples

import zio.prelude._

// scalafix:off
object CustomFunctionExample {
  object Palindrome extends Newtype[String] {
    override def assertion = assertCustom { str =>
      if (str.reverse == str) Right(()) else Left(AssertionError.Failure("isPalindrome"))
    }
  }

  Palindrome("racecar")
}
// scalafix:on
