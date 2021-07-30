package zio.prelude
package scalaparallelcollections

import com.github.ghik.silencer.silent
import zio.{Has, Random}
import zio.test._
import zio.test.laws._

import scala.collection.parallel.{immutable => par}

@silent("Unused import")
object IdentityFlattenJvmSpec extends DefaultRunnableSpec {
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

  val genParSeq: GenF[Has[Random] with Has[Sized], par.ParSeq] =
    new GenF[Has[Random] with Has[Sized], par.ParSeq] {
      def apply[R1 <: Has[Random] with Has[Sized], A](gen: Gen[R1, A]): Gen[R1, par.ParSeq[A]] =
        Gen.listOf(gen).map(_.par)
    }

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityFlattenJvmSpec")(
      suite("laws")(
        test("parSeq")(checkAllLaws(IdentityFlatten)(genParSeq, Gen.anyInt))
      )
    )
}
