package zio.prelude
package scalaparallelcollections

import com.github.ghik.silencer.silent
import zio.prelude.laws._
import zio.random.Random
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

  val genParSeq: GenF[Random with Sized, par.ParSeq] =
    new GenF[Random with Sized, par.ParSeq] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, par.ParSeq[A]] =
        Gen.listOf(gen).map(_.par)
    }

  def spec: ZSpec[Environment, Failure] =
    suite("IdentityFlattenJvmSpec")(
      suite("laws")(
        testM("parSeq")(checkAllLaws(IdentityFlattenLaws)(genParSeq, Gen.anyInt))
      )
    )
}
