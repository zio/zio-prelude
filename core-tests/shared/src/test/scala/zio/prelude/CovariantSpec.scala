package zio.prelude

import zio.Chunk
import zio.prelude.laws._
import zio.test._
import zio.test.laws._

object CovariantSpec extends ZIOSpecDefault {
  import Fixtures._

  implicit val chunkOptionCovariant: Covariant[ChunkOption] =
    Covariant[Chunk].compose(Covariant[Option])

  def spec: ZSpec[Environment, Any] =
    suite("CovariantSpec")(
      suite("laws")(
        test("cause")(checkAllLaws(CovariantLaws)(GenFs.cause, Gen.int)),
        test("exit")(checkAllLaws(CovariantLaws)(GenFs.exit(Gen.causes(Gen.int, Gen.throwable)), Gen.int)),
        test("tuple2")(checkAllLaws(CovariantLaws)(GenFs.tuple2(Gen.int), Gen.int)),
        test("tuple3")(checkAllLaws(CovariantLaws)(GenFs.tuple3(Gen.int, Gen.int), Gen.int))
      )
    )
}
