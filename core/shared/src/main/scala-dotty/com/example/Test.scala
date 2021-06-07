package com.example

object Test extends App {
  import Assertion.*
  import Regex.*

  type Natural <: Int

  // MUST be defined as transparent
  // https://github.com/lampepfl/dotty/issues/12368
  transparent inline def Natural: Int Refined Natural = Refined {
    greaterThanEqual(0) && lessThanEqual(100)
  }

  type Age <: Int
  transparent inline def Age: Int Refined Age = Refined {
    greaterThanEqual(0) && lessThanEqual(150)
  }

  val natural: Natural = Natural(5)

  val age: Age = Age(60)

  def add(a: Int, b: Int): Int = a + b

  println(add(natural, age))

  val x: Natural                 = Natural(0)
  val y: Either[String, Natural] = Natural(scala.util.Random.nextInt)

  type Email <: String
  transparent inline def Email: String Refined Email = Refined {
    matches {
      char(isAlpha || isDigit).min(1)
        ~ char(equal('@'))
        ~ char(isAlpha || isDigit).min(1)
        ~ char(equal('.'))
        ~ char(isAlpha).min(1)
    }
  }
}

// val e1: Email = Email("jorge@scalac.io")

// def add(a: Natural, b: Natural): Natural = ???
