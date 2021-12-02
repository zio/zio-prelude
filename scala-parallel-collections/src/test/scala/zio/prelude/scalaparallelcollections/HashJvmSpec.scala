package zio.prelude
package scalaparallelcollections

import com.github.ghik.silencer.silent
import zio.prelude.HashSpec.scalaHashCodeConsistency
import zio.prelude.laws._
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
        testM("parMap")(checkAllLaws(HashLaws)(Gen.mapOf(Gen.anyInt, Gen.anyInt).map(_.par))),
        testM("parSeq")(checkAllLaws(HashLaws)(Gen.listOf(Gen.anyInt).map(_.par))),
        testM("parSet")(checkAllLaws(HashLaws)(Gen.setOf(Gen.anyInt).map(_.par)))
      ),
      suite("ScalaHashCode consistency")(
        testM("parMap")(scalaHashCodeConsistency(Gen.mapOf(Gen.anyInt, Gen.anyInt).map(_.par))),
        testM("parSeq")(scalaHashCodeConsistency(Gen.listOf(Gen.anyInt).map(_.par))),
        testM("parSet")(scalaHashCodeConsistency(Gen.setOf(Gen.anyInt).map(_.par)))
      )
    )
}
