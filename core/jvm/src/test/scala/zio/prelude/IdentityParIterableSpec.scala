package zio.prelude

import zio.blocking.Blocking
import zio.prelude.newtypes.Sum
import zio.test._

import com.github.ghik.silencer.silent

@silent
object IdentityParIterableSpec extends DefaultRunnableSpec {
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

  val spec: Spec[Blocking, TestFailure[Throwable], TestSuccess] =
    suite("ParIterable")(
      test("ParIterable non-empty returns a value") {
        assert(List(Sum(1), Sum(2), Sum(3), Sum(4)).par.reduceIdentity)(equalTo(Sum(10)))
      },
      testM("ParIterable empty returns the `identity` element, non-blocking") {
        assertM(List[Sum[Int]]().par.reduceIdentityM)(equalTo(Sum(0)))
      }
    )
}
