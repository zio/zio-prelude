package zio.prelude

// scalafix:off
object NewtypeSpecTypes212 {

  object Palindrome extends Newtype[String] {
    override def assertion = assertCustom { str =>
      if (str.reverse == str) Right(()) else Left(AssertionError.Failure("isPalindrome"))
    }
  }

  Palindrome("racecar")
}
// scalafix:on
