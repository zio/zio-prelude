package zio.prelude

import zio.test._
import zio.test.laws._

object CovariantSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("CovariantSpec")(
      suite("laws")(
        testM("option")(checkAllLaws(Covariant)(GenF.option, Gen.anyInt)),
        testM("list")(checkAllLaws(Covariant)(GenF.list, Gen.anyInt)),
        testM("vector")(checkAllLaws(Covariant)(GenF.vector, Gen.anyInt)),
        testM("map")(checkAllLaws(Covariant)(GenFs.map(Gen.anyInt), Gen.anyInt)),
        testM("either")(checkAllLaws(Covariant)(GenFs.either(Gen.anyInt), Gen.anyInt)),
        testM("tuple2")(checkAllLaws(Covariant)(GenFs.tuple2(Gen.anyInt), Gen.anyInt)),
        testM("tuple3")(checkAllLaws(Covariant)(GenFs.tuple3(Gen.anyInt, Gen.anyInt), Gen.anyInt)),
        testM("cause")(checkAllLaws(Covariant)(GenFs.cause, Gen.anyInt)),
        testM("chunk")(checkAllLaws(Covariant)(GenF.chunk, Gen.anyInt)),
        testM("exit")(checkAllLaws(Covariant)(GenFs.exit(Gen.causes(Gen.anyInt, Gen.throwable)), Gen.anyInt)),
        testM("nonEmptyChunk")(checkAllLaws(Covariant)(GenFs.nonEmptyChunk, Gen.anyInt)),
        testM("Nested[vector,cause]")(checkAllLaws(Covariant)(GenFs.nested(GenF.vector, GenFs.cause), Gen.anyInt))
      )
    )
}
