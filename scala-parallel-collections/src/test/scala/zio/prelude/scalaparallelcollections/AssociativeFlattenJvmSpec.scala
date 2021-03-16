package zio.prelude
package scalaparallelcollections

import com.github.ghik.silencer.silent
import zio.random.Random
import zio.test._
import zio.test.laws._

import scala.collection.parallel.{immutable => par}

@silent("Unused import")
object AssociativeFlattenJvmSpec extends DefaultRunnableSpec {
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

  def genParMap[R <: Random with Sized, K](k: Gen[R, K]): GenF[R, ({ type lambda[+v] = par.ParMap[K, v] })#lambda] =
    new GenF[R, ({ type lambda[+v] = par.ParMap[K, v] })#lambda] {
      def apply[R1 <: R, V](v: Gen[R1, V]): Gen[R1, par.ParMap[K, V]] =
        Gen.mapOf(k, v).map(_.par)
    }

  def spec: ZSpec[Environment, Failure] =
    suite("AssociativeFlattenJvmSpec")(
      suite("laws")(
        testM("parMap")(checkAllLaws(AssociativeFlatten)(genParMap(Gen.anyInt), Gen.anyInt))
      )
    )
}
