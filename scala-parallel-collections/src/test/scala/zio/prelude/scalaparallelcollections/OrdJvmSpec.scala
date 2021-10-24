package zio.prelude
package scalaparallelcollections

import com.github.ghik.silencer.silent
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

@silent("Unused import")
object OrdJvmSpec extends DefaultRunnableSpec {
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
    suite("OrdJvmSpec")(
      suite("laws")(
        testM("parSeq")(checkAllLaws(OrdLaws)(Gen.listOf(Gen.anyInt).map(_.par)))
      )
    )
}
