package zio.prelude
package scalaparallelcollections

import com.github.ghik.silencer.silent
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

@silent("Unused import")
object PartialOrdJvmSpec extends ZIOSpecDefault {
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
    suite("EqualJvmSpec")(
      suite("laws")(
        test("parMap")(checkAllLaws(PartialOrdLaws)(Gen.mapOf(Gen.int, Gen.int).map(_.par))),
        test("parSet")(checkAllLaws(PartialOrdLaws)(Gen.setOf(Gen.int).map(_.par)))
      )
    )
}
