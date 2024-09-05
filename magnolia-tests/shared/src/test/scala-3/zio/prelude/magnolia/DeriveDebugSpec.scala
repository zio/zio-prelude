package zio.prelude.magnolia

import zio.Scope
import zio.prelude._
import zio.test.Assertion._
import zio.test.{ZIOSpecDefault, _}

object DeriveDebugSpec extends ZIOSpecDefault {

  case class Lair(name: String, animal: Animal) derives Debug

  case class Animal(name: String, age: Int)  derives Debug

  case class Adult(name: String, age: Int, chidren: List[Child]) derives Debug
  case class Child(name: String, age: Int) derives Debug

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("DeriveDebug with derives")(
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
