package zio.prelude
package scalaparallelcollections

import com.github.ghik.silencer.silent
import zio.prelude.laws._
import zio.test._
import zio.test.laws._
import zio.{Random, ZTraceElement}

import scala.collection.parallel.{immutable => par}

@silent("Unused import")
object ForEachJvmSpec extends DefaultRunnableSpec {
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

  def genParMap[R <: Random with Sized, K](
    k: Gen[R, K]
  ): GenF[R, ({ type lambda[+v] = par.ParMap[K, v] })#lambda] =
    new GenF[R, ({ type lambda[+v] = par.ParMap[K, v] })#lambda] {
      def apply[R1 <: R, V](v: Gen[R1, V])(implicit trace: ZTraceElement): Gen[R1, par.ParMap[K, V]] =
        Gen.mapOf(k, v).map(_.par)
    }

  val genParSeq: GenF[Random with Sized, par.ParSeq] =
    new GenF[Random with Sized, par.ParSeq] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A])(implicit
        trace: ZTraceElement
      ): Gen[R1, par.ParSeq[A]] =
        Gen.listOf(gen).map(_.par)
    }

  def spec: ZSpec[Environment, Failure] =
    suite("ForEachJvmSpec")(
      suite("laws")(
        test("parMap")(checkAllLaws(ForEachLaws)(genParMap(Gen.int), Gen.int)),
        test("parSeq")(checkAllLaws(ForEachLaws)(genParSeq, Gen.int))
      )
    )
}
