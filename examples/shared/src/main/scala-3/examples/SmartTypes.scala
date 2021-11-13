package examples

import zio.prelude.*

object SmartTypes extends App {

  type Natural = Natural.Type
  object Natural extends SubtypeSmart[Int] {
    override inline def validateInline(inline value: Int) =
      ${ NaturalValidator.validateInlineImpl('value) }

    override def validate(value: Int) =
      NaturalValidator.validate(value)
    extension (self: Natural) {
      infix def add(that: Natural): Natural = wrap(unwrap(self) + unwrap(that))
    }
  }

  type Age = Age.Type
  object Age extends SubtypeSmart[Int] {
    override inline def validateInline(inline value: Int) =
      ${ AgeValidator.validateInlineImpl('value) }

    override def validate(value: Int) =
      AgeValidator.validate(value)
  }

  val natural: Natural = Natural(5) add Natural(8)

  val age: Age = Age(60)

  def sum(a: Int, b: Int): Int = a + b

  println(sum(natural, age))

  val x: Natural                 = Natural(0)
  // val y: Either[String, Natural] = Natural.unsafeWrap(scala.util.Random.nextInt)

  type MyRegex = MyRegex.Type
  object MyRegex extends SubtypeSmart[String] {
    override inline def validateInline(inline value: String) =
      ${ MyRegexValidator.validateInlineImpl('value) }

    override def validate(value: String) =
      MyRegexValidator.validate(value)
  }

  val myRegex: MyRegex = MyRegex("ab#l*helloccayj678")

  object Email extends Subtype[String] {
    override inline def assertion = {
      matches {
        start
           ~ anyRegexOf(alphanumeric, literal("-"), literal("\\.")).+
           ~ literal("@")
           ~ anyRegexOf(alphanumeric, literal("-")).+
           ~ literal("\\.").+
           ~ anyRegexOf(alphanumeric, literal("-")).between(2, 4)
           ~ end
      }
    }
  }
  type Email = Email.Type
  val email: Email = Email("test@test.com")
}
