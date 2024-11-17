package zio.prelude.data

import zio.Scope
import zio.prelude.ZIOBaseSpec
import zio.test.{Spec, TestEnvironment, assertTrue}

object OptionalSpec extends ZIOBaseSpec {

  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Optional")(
      test("is an IterableOnce") {
        def get(a: Int): Optional[Long] = Optional.Present(a.toLong)
        val list: List[Int]             = List(1, 2, 3)
        val result: List[Long]          = list.flatMap(get)

        assertTrue(result == List(1L, 2L, 3L))
      }
    )

}
