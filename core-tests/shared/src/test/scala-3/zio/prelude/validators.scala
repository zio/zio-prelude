package zio.prelude

import zio.prelude._

object PalindromeValidator
    extends Validator[String](str =>
      if (str.reverse == str) Right(()) else Left(AssertionError.Failure("isPalindrome"))
    )

object NonEmptyStringValidator
    extends Validator[String]((s: String) =>
      if (s.isBlank) Left(AssertionError.failure("NonEmptyString cannot be empty"))
      else Right(())
    )
