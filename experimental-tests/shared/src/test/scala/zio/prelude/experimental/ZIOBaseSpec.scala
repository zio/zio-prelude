package zio.prelude.experimental

import zio._
import zio.test.TestAspect._
import zio.test._

trait ZIOBaseSpec extends ZIOSpecDefault {
  override def aspects: Chunk[TestAspectAtLeastR[Environment with TestEnvironment]] =
    if (TestPlatform.isJVM) Chunk(TestAspect.timeout(60.seconds), TestAspect.timed)
    else if (TestPlatform.isNative) Chunk(TestAspect.timeout(300.seconds), TestAspect.timed, size(10))
    else Chunk(TestAspect.timeout(300.seconds), TestAspect.sequential, TestAspect.timed, size(10))
}
