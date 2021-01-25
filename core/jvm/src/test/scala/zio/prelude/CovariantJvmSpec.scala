package zio.prelude

import zio.random.Random
import zio.test._
import zio.test.laws._

import scala.collection.parallel.{immutable => par}

object CovariantJvmSpec extends DefaultRunnableSpec {

  def genParMap[R <: Random with Sized, K](k: Gen[R, K]): GenF[R, ({ type lambda[+v] = par.ParMap[K, v] })#lambda] =
    new GenF[R, ({ type lambda[+v] = par.ParMap[K, v] })#lambda] {
      def apply[R1 <: R, V](v: Gen[R1, V]): Gen[R1, par.ParMap[K, V]] =
        Gen.mapOf(k, v).map(_.par)
    }

  val genParSeq: GenF[Random with Sized, par.ParSeq] =
    new GenF[Random with Sized, par.ParSeq] {
      def apply[R1 <: Random with Sized, A](gen: Gen[R1, A]): Gen[R1, par.ParSeq[A]] =
        Gen.listOf(gen).map(_.par)
    }

  def spec: ZSpec[Environment, Failure] =
    suite("CovariantJvmSpec")(
      suite("laws")(
        testM("parMap")(checkAllLaws(Covariant)(genParMap(Gen.anyInt), Gen.anyInt)),
        testM("parSeq")(checkAllLaws(Covariant)(genParSeq, Gen.anyInt))
      )
    )
}
