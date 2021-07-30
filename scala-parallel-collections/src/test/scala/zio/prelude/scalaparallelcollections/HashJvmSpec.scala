package zio.prelude
package scalaparallelcollections

import com.github.ghik.silencer.silent
import zio.prelude.HashSpec.scalaHashCodeConsistency
import zio.test._
import zio.test.laws._

@silent("Unused import")
object HashJvmSpec extends DefaultRunnableSpec {
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

  def spec: ZSpec[Environment, Failure] =
    suite("HashJvmSpec")(
      suite("laws")(
        test("parMap")(checkAllLaws(Hash)(Gen.mapOf(Gen.anyInt, Gen.anyInt).map(_.par))),
        test("parSeq")(checkAllLaws(Hash)(Gen.listOf(Gen.anyInt).map(_.par))),
        test("parSet")(checkAllLaws(Hash)(Gen.setOf(Gen.anyInt).map(_.par)))
      ),
      suite("ScalaHashCode consistency")(
        test("parMap")(scalaHashCodeConsistency(Gen.mapOf(Gen.anyInt, Gen.anyInt).map(_.par))),
        test("parSeq")(scalaHashCodeConsistency(Gen.listOf(Gen.anyInt).map(_.par))),
        test("parSet")(scalaHashCodeConsistency(Gen.setOf(Gen.anyInt).map(_.par)))
      )
    )
}
