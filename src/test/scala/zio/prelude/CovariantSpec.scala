package zio.prelude

import zio.Exit
import zio.random.Random
import zio.test._
import zio.test.laws._

object CovariantSpec extends DefaultRunnableSpec {

  val genFMap: GenF[Random with Sized, ({ type lambda[+x] = Map[Int, x] })#lambda] =
    GenF.map(Gen.anyInt)

  val genFEither: GenF[Random with Sized, ({ type lambda[+x] = Either[Int, x] })#lambda] =
    GenF.either(Gen.anyInt)

  val genFTuple: GenF[Random with Sized, ({ type lambda[+x] = (Int, x) })#lambda] =
    GenFs.tuple2(Gen.anyInt)

  val genFTuple3: GenF[Random with Sized, ({ type lambda[+x] = ((Int, Int), x) })#lambda] =
    GenFs.tuple3(Gen.anyInt.zip(Gen.anyInt))

  val genFExit: GenF[Random with Sized, ({ type lambda[+a] = Exit[Int, a] })#lambda] =
    GenFs.exit(Gen.anyInt)

  def spec = suite("CovariantSpec")(
    suite("laws")(
      testM("option")(checkAllLaws(Covariant)(GenF.option, Gen.anyInt)),
      testM("list")(checkAllLaws(Covariant)(GenF.list, Gen.anyInt)),
      testM("vector")(checkAllLaws(Covariant)(GenF.vector, Gen.anyInt)),
      testM("map")(checkAllLaws(Covariant)(genFMap, Gen.anyInt)),
      testM("either")(checkAllLaws(Covariant)(genFEither, Gen.anyInt)),
      testM("tuple2")(checkAllLaws(Covariant)(genFTuple, Gen.anyInt)),
      testM("tuple3")(checkAllLaws(Covariant)(genFTuple3, Gen.anyInt)),
      testM("cause")(checkAllLaws(Covariant)(GenFs.fail, Gen.anyString)),
      testM("chunk")(checkAllLaws(Covariant)(GenF.chunk, Gen.anyInt)),
      testM("exit")(checkAllLaws(Covariant)(genFExit, Gen.anyInt)),
      testM("nonEmptyChunk")(checkAllLaws(Covariant)(GenFs.nonEmptyChunk, Gen.anyInt))
    )
  )
}
