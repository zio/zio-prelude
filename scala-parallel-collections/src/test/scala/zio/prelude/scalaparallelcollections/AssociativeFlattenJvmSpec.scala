package zio.prelude
package scalaparallelcollections

import com.github.ghik.silencer.silent
import zio.Trace
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

import scala.collection.parallel.{immutable => par}

@silent("Unused import")
object AssociativeFlattenJvmSpec extends ZIOBaseSpec {
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

  def spec: Spec[Environment, Any] =
    suite("AssociativeFlattenJvmSpec")(
      suite("laws")(
        test("parMap")(checkAllLaws(AssociativeFlattenLaws)(genParMap(Gen.int), Gen.int))
      )
    )
}
