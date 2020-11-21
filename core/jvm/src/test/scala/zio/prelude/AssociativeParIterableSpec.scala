package zio.prelude

import zio.blocking.Blocking
import zio.prelude.newtypes.Sum
import zio.test._

import com.github.ghik.silencer.silent

@silent
object AssociativeParIterableSpec extends DefaultRunnableSpec {
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
      testM("ParIterable non-empty returns Some, non-blocking") {
        assertM(List(Sum(1), Sum(2), Sum(3), Sum(4)).par.reduceAssociativeM)(equalTo(Some(Sum(10))))
      },
      test("ParIterable empty returns None") {
        assert(List[Sum[Int]]().par.reduceAssociative)(equalTo(None))
      }
    )
}
