package zio.prelude.derivation.tests

import zio._
import zio.prelude._
import zio.prelude.derivation.DeriveDebug
import zio.test._

object DeriveDebugSpec extends ZIOSpecDefault {
  sealed trait Entity
  object Entity {
    case class Person(name: String, address: Address) extends Entity
    object Person {
      implicit val debug: Debug[Person] = DeriveDebug.gen[Person]
    }

    case class Organization(name: String, contacts: List[Person]) extends Entity

    // need to keep it as not implicit, or the derivation would reference itself for the subtype's instances
    // and runs into StackOverflow in the runtime
    val debug: Debug[Entity] = DeriveDebug.gen[Entity]
  }

  case class Address(lines: List[String], country: Country)
  object Address {
    implicit val debug: Debug[Address] = DeriveDebug.gen[Address]
  }

  case class Country(name: String, code: String, salesTax: Boolean)
  object Country {
    implicit val debug: Debug[Country] = DeriveDebug.gen[Country]
  }

  case object ExampleObject {
    implicit val debug: Debug[ExampleObject.type] = DeriveDebug.gen[ExampleObject.type]
  }

  case class ValueClass(value: String) extends AnyVal
  object ValueClass {
    implicit val debug: Debug[ValueClass] = DeriveDebug.gen[ValueClass]
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("DeriveDebugSpec")(
    test("should be able to derive Debug instance for case classes and sealed traits") {
      implicit val implicitDebug: Debug[Entity] = Entity.debug

      val caseClassSample            = Address(List("foo"), Country("bar", "baz", salesTax = true))
      val sealedTraitExample: Entity = Entity.Organization("some-org", List(Entity.Person("qux", caseClassSample)))

      assertTrue(sealedTraitExample.render.nonEmpty)
    },
    test("should be able to derive Debug instance for case object") {
      assertTrue(ExampleObject.render.nonEmpty)
    },
    test("should be able to derive Debug instance for value class") {
      assertTrue(ValueClass("sample-value-class").render.nonEmpty)
    }
  )

}
