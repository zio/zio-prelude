package zio.prelude
package scalaparallelcollections

import com.github.ghik.silencer.silent
import zio.Trace
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

import scala.collection.parallel.{immutable => par}

@silent("Unused import")
object IdentityFlattenJvmSpec extends ZIOBaseSpec {
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

  val genParSeq: GenF[Sized, par.ParSeq] =
    new GenF[Sized, par.ParSeq] {
      def apply[R1 <: Sized, A](gen: Gen[R1, A])(implicit
        trace: Trace
      ): Gen[R1, par.ParSeq[A]] =
        Gen.listOf(gen).map(_.par)
    }

  def spec: Spec[Environment, Any] =
    suite("IdentityFlattenJvmSpec")(
      suite("laws")(
        test("parSeq")(checkAllLaws(IdentityFlattenLaws)(genParSeq, Gen.int))
      )
    )
}
