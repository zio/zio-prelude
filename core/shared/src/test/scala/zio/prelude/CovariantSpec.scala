package zio.prelude

import zio.Chunk
import zio.test._
import zio.test.laws._

object CovariantSpec extends DefaultRunnableSpec {
  import Fixtures._

  implicit val chunkOptionCovariant: Covariant[ChunkOption] =
    Covariant[Chunk].compose(Covariant[Option])

  def spec: ZSpec[Environment, Failure] =
    suite("CovariantSpec")(
      suite("laws")(
        testM("cause")(checkAllLaws(Covariant)(GenFs.cause, Gen.anyInt)),
        testM("exit")(checkAllLaws(Covariant)(GenFs.exit(Gen.causes(Gen.anyInt, Gen.throwable)), Gen.anyInt)),
        testM("tuple2")(checkAllLaws(Covariant)(GenFs.tuple2(Gen.anyInt), Gen.anyInt)),
        testM("tuple3")(checkAllLaws(Covariant)(GenFs.tuple3(Gen.anyInt, Gen.anyInt), Gen.anyInt))
      )
    )
}
