package zio.prelude
package scalaparallelcollections

import scala.annotation.nowarn
import zio.prelude.HashSpec.scalaHashCodeConsistency
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

@nowarn("msg=Unused import")
object HashJvmSpec extends ZIOBaseSpec {
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
    suite("HashJvmSpec")(
      suite("laws")(
        test("parMap")(checkAllLaws(HashLaws)(Gen.mapOf(Gen.int, Gen.int).map(_.par))),
        test("parSeq")(checkAllLaws(HashLaws)(Gen.listOf(Gen.int).map(_.par))),
        test("parSet")(checkAllLaws(HashLaws)(Gen.setOf(Gen.int).map(_.par)))
      ),
      suite("ScalaHashCode consistency")(
        test("parMap")(scalaHashCodeConsistency(Gen.mapOf(Gen.int, Gen.int).map(_.par))),
        test("parSeq")(scalaHashCodeConsistency(Gen.listOf(Gen.int).map(_.par))),
        test("parSet")(scalaHashCodeConsistency(Gen.setOf(Gen.int).map(_.par)))
      )
    )
}
