package zio.prelude

import com.github.ghik.silencer.silent
import zio.test._
import zio.test.laws._

@silent("Unused import")
object PartialOrdJvmSpec extends DefaultRunnableSpec {
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
        testM("parMap")(checkAllLaws(PartialOrd)(Gen.mapOf(Gen.anyInt, Gen.anyInt).map(_.par))),
        testM("parSet")(checkAllLaws(PartialOrd)(Gen.setOf(Gen.anyInt).map(_.par)))
      )
    )
}
