package zio.prelude

import zio.Scope
import zio.test._

object NonEmptyMapSpec extends ZIOBaseSpec {

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("NonEmptyMapSpec")(
      suite("++")(
        test("adds pairs and replaces values of existing keys") {
          val a = NonEmptyMap(1 -> "a", 2 -> "b") ++ List(2 -> "c", 3 -> "d")
          assertTrue(
            a == NonEmptyMap(1 -> "a", 2 -> "c", 3 -> "d")
          )
        }
      ),
      suite("NonEmptySortedMap ++")(
        test("adds pairs and replaces values of existing keys") {
          val a = NonEmptySortedMap(1 -> "a", 2 -> "b") ++ List(2 -> "c", 3 -> "d")
          assertTrue(
            a == NonEmptySortedMap(1 -> "a", 2 -> "c", 3 -> "d")
          )
        }
      )
    )
}
