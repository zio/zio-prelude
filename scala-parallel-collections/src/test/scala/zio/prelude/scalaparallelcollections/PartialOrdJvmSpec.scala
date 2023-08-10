package zio.prelude
package scalaparallelcollections

import scala.annotation.nowarn
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

@nowarn("msg=Unused import")
object PartialOrdJvmSpec extends ZIOBaseSpec {
  private val ParallelCollectionCompatibility = {
    object Compat {
      object CollectionConverters
    }
    import Compat._
    {
      import scala.collection.parallel._
      CollectionConverters
    }
  }
  import ParallelCollectionCompatibility._

  def spec: Spec[Environment, Any] =
    suite("EqualJvmSpec")(
      suite("laws")(
        test("parMap")(checkAllLaws(PartialOrdLaws)(Gen.mapOf(Gen.int, Gen.int).map(_.par))),
        test("parSet")(checkAllLaws(PartialOrdLaws)(Gen.setOf(Gen.int).map(_.par)))
      )
    )
}
