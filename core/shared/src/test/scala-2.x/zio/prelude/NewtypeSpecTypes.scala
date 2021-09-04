// scalafix:off
package zio.prelude

import zio.prelude.Refinement.Regex._

object NewtypeSpecTypes {

  type Natural = Natural.Type
  object Natural extends Subtype[Int] {
    def refinement =
      refine(Refinement.greaterThanOrEqualTo(0))

    val two: Natural = Natural(2)

    def unsafeWrap(int: Int): Natural = wrap(int)
  }

  type LuckyNumber = LuckyNumber.Type
  object LuckyNumber extends Newtype[Double] {
    def refinement =
      refine(Refinement.between(10.0, 20.0))
  }

  LuckyNumber(19.0)

  // A scala.util.matching.Regex Validated Email
  type Email = Email.Type
  object Email extends Newtype[String] {

    // Mega Email Regex pilfered from https://stackoverflow.com/a/201378
    def refinement =
      refine(
        Refinement.matches(
          "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])".r
        )
      )
  }

  // These should compile
  Email("very.cool.person@gmail.com")
  Email("bobo123+fancy@clown.gov")

  // A String Regex Validated Username
  type Username = Username.Type
  object Username extends Newtype[String] {

    def refinement =
      refine(Refinement.matches("\\w{3,8}\\d\\d"))
  }

  // These should compile
  Username("fancy12")
  Username("lovely89")

  // An Refinement.Regex Validated Email
  type Password = Password.Type
  object Password extends Newtype[String] {

    def refinement =
      refine(Refinement.matches(literal("a").+ ~ anyChar.between(1, 3) ~ literal("oh").*))
  }

  // These should compile
  Password("aaaaaaaaa134ohohoh")
  Password("ahoe")
  Password("a1oh")

  type PowerOfTwo = PowerOfTwo.Type
  object PowerOfTwo extends Newtype[Int] {
    def refinement =
      refine(Refinement.powerOf(2))
  }

  PowerOfTwo(1024)

  type Pin = Pin.Type
  object Pin extends Subtype[Int] {
    def refinement =
      refine(Refinement.between(1000, 9999))
  }

  Pin(1234)
  Pin(9999)
  Pin(1000)

}
// scalafix:on
