package examples

import zio.prelude._

object PalindromeValidator extends Validator[String](str =>
  if (str.reverse == str) Right(()) else Left(AssertionError.Failure("isPalindrome"))
)
