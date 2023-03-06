package zio.prelude

import zio._
import zio.test._

trait ZIOBaseSpec extends ZIOSpecDefault {
  override def aspects: Chunk[TestAspectAtLeastR[Environment with TestEnvironment]] =
    if (TestPlatform.isJVM) Chunk(TestAspect.timeout(60.seconds), TestAspect.timed)
    else Chunk(TestAspect.timeout(60.seconds), TestAspect.sequential, TestAspect.timed)
}