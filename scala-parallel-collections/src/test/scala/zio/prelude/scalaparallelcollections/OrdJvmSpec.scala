package zio.prelude
package scalaparallelcollections

import scala.annotation.nowarn
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

@nowarn("msg=Unused import")
object OrdJvmSpec extends ZIOBaseSpec {
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
    suite("OrdJvmSpec")(
      suite("laws")(
        test("parSeq")(checkAllLaws(OrdLaws)(Gen.listOf(Gen.int).map(_.par)))
      )
    )
}
