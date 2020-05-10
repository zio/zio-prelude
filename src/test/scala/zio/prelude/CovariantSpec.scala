package zio.prelude

import zio.random.Random
import zio.test.{ Gen, _ }
import zio.test.laws._

object CovariantSpec extends DefaultRunnableSpec {

  val genFMap: GenF[Random with Sized, ({ type lambda[+x] = Map[Int, x] })#lambda] =
    GenFs.map(Gen.anyInt)

  val genFEither: GenF[Random with Sized, ({ type lambda[+x] = Either[Int, x] })#lambda] =
    GenFs.either(Gen.anyInt)

  val genFTuple: GenF[Random with Sized, ({ type lambda[+x] = (Int, x) })#lambda] =
    GenFs.tuple2(Gen.anyInt)

  val genFTuple3: GenF[Random with Sized, ({ type lambda[+x] = ((Int, Int), x) })#lambda] =
    GenFs.tuple3(Gen.anyInt.zip(Gen.anyInt))

  def spec = suite("CovariantSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(Covariant)(GenFs.option, Gen.anyInt)),
      testM("list")(checkAllLaws(Covariant)(GenFs.list, Gen.anyInt)),
      testM("vector")(checkAllLaws(Covariant)(GenFs.vector, Gen.anyInt)),
      testM("map")(checkAllLaws(Covariant)(genFMap, Gen.anyInt)),
      testM("either")(checkAllLaws(Covariant)(genFEither, Gen.anyInt)),
      testM("tuple2")(checkAllLaws(Covariant)(genFTuple, Gen.anyInt)),
      testM("tuple3")(checkAllLaws(Covariant)(genFTuple3, Gen.anyInt))
    )
  )
}
