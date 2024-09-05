package zio.prelude.magnolia

import zio.Scope
import zio.prelude._
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

object DeriveDebugImplictsSpec extends ZIOSpecDefault {

  case class Lair(name: String, animal: Animal)
  object Lair {
    implicit val debug: Debug[Lair] = DeriveDebug.derived[Lair]
  }

  case class Animal(name: String, age: Int)

  object Animal {
    implicit val debug: Debug[Animal] = DeriveDebug.derived[Animal]
  }

  case class Adult(name: String, age: Int, chidren: List[Child])
  case class Child(name: String, age: Int)

  object Adult {
    implicit val debug: Debug[Adult] = DeriveDebug.derived[Adult]
  }

  object Child {
    implicit val debug: Debug[Child] = DeriveDebug.derived[Child]
  }

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("DeriveDebug with implicits")(
      test("should derive Debug for case class") {
        val animal = Animal("tiger", 10)

        assert(animal.debug.render)(equalTo("Animal(name = \"tiger\", age = 10)"))

      },
      test("should derive Debug for nested case class") {
        val lair = Lair("jungle", Animal("tiger", 10))

        assert(lair.debug.render)(equalTo("Lair(name = \"jungle\", animal = Animal(name = \"tiger\", age = 10))"))
      },
      test("should derive Debug for case class with list") {
        val adult = Adult("John", 30, List(Child("Alice", 5), Child("Bob", 10)))

        assert(adult.debug.render)(
          equalTo(
            "Adult(name = \"John\", age = 30, chidren = List(Child(name = \"Alice\", age = 5), Child(name = \"Bob\", age = 10)))"
          )
        )
      }
    )
}
