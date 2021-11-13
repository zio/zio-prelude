package zio.prelude

import zio.prelude.Assertion.Regex.*

object NewtypeSpecTypes {

  type Natural = Natural.Type
  object Natural extends SubtypeSmart[Int] {
    override inline def validateInline(inline value: Int) =
      ${ NaturalValidator.validateInlineImpl('value) }

    override def validate(value: Int) =
      NaturalValidator.validate(value)

    val two: Natural = Natural(2)

    def unsafeWrap(int: Int): Natural = wrap(int)
  }

  type LuckyNumber = LuckyNumber.Type
  object LuckyNumber extends NewtypeSmart[Double] {
    override inline def validateInline(inline value: Double) =
      ${ LuckyNumberValidator.validateInlineImpl('value) }

    override def validate(value: Double) =
      LuckyNumberValidator.validate(value)
  }

  LuckyNumber(19.0)
  LuckyNumber(10, 11, 12, 13.5)

  // A scala.util.matching.Regex Validated Email
  type Email = Email.Type
  object Email extends NewtypeSmart[String] {
    override inline def validateInline(inline value: String) =
      ${ EmailValidator.validateInlineImpl('value) }

    override def validate(value: String) =
      EmailValidator.validate(value)
  }

  // These should compile
  Email("very.cool.person@gmail.com")
  Email("bobo123+fancy@clown.gov")
  Email("a@b.com", "c@d.net")

  // A String Regex Validated Username
  type Username = Username.Type
  object Username extends NewtypeSmart[String] {
    override inline def validateInline(inline value: String) =
      ${ UsernameValidator.validateInlineImpl('value) }

    override def validate(value: String) =
      UsernameValidator.validate(value)
  }

  // These should compile
  Username("fancy12")
  Username("lovely89")
  Username("lovely89", "oeu2881321")

  // An Assertion.Regex Validated Email
  type Password = Password.Type
  object Password extends NewtypeSmart[String] {
    override inline def validateInline(inline value: String) =
      ${ PasswordValidator.validateInlineImpl('value) }

    override def validate(value: String) =
      PasswordValidator.validate(value)
  }

  // These should compile
  Password("aaaaaaaaa134ohohoh")
  Password("aaaaaaoeoh")
  Password("aaaaaaoeoh", "aoh")
  Password("a1oh")

  type PowerOfTwo = PowerOfTwo.Type
  object PowerOfTwo extends NewtypeSmart[Int] {
    override inline def validateInline(inline value: Int) =
      ${ PowerOfTwoValidator.validateInlineImpl('value) }

    override def validate(value: Int) =
      PowerOfTwoValidator.validate(value)
  }

  PowerOfTwo(1024)
  PowerOfTwo(1024, 64)

  type Pin = Pin.Type
  object Pin extends SubtypeSmart[Int] {
    override inline def validateInline(inline value: Int) =
      ${ PinValidator.validateInlineImpl('value) }

    override def validate(value: Int) =
      PinValidator.validate(value)
  }

  Pin(9999)
  Pin(1000)
  Pin(1000, 1001, 9998, 9999)

}
