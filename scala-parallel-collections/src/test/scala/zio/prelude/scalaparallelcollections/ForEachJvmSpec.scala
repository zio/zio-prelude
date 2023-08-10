package zio.prelude
package scalaparallelcollections

import scala.annotation.nowarn
import zio.Trace
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

import scala.collection.parallel.{immutable => par}

@nowarn("msg=Unused import")
object ForEachJvmSpec extends ZIOBaseSpec {
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

  def genParMap[R <: Sized, K](
    k: Gen[R, K]
  ): GenF[R, ({ type lambda[+v] = par.ParMap[K, v] })#lambda] =
    new GenF[R, ({ type lambda[+v] = par.ParMap[K, v] })#lambda] {
      def apply[R1 <: R, V](v: Gen[R1, V])(implicit trace: Trace): Gen[R1, par.ParMap[K, V]] =
        Gen.mapOf(k, v).map(_.par)
    }

  val genParSeq: GenF[Sized, par.ParSeq] =
    new GenF[Sized, par.ParSeq] {
      def apply[R1 <: Sized, A](gen: Gen[R1, A])(implicit
        trace: Trace
      ): Gen[R1, par.ParSeq[A]] =
        Gen.listOf(gen).map(_.par)
    }

  def spec: Spec[Environment, Any] =
    suite("ForEachJvmSpec")(
      suite("laws")(
        test("parMap")(checkAllLaws(ForEachLaws)(genParMap(Gen.int), Gen.int)),
        test("parSeq")(checkAllLaws(ForEachLaws)(genParSeq, Gen.int))
      )
    )
}
