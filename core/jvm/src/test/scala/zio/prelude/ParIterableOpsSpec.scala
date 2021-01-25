package zio.prelude

import com.github.ghik.silencer.silent
import zio.blocking.Blocking
import zio.prelude.newtypes.Sum
import zio.test._

@silent("Unused import")
object ParIterableOpsSpec extends DefaultRunnableSpec {
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

  def spec: Spec[Blocking, TestFailure[Throwable], TestSuccess] =
    suite("ParIterableOpsSpec")(
      testM("reduceCommutativeM: non-empty returns Some, non-blocking") {
        assertM(List(Sum(1), Sum(2), Sum(3), Sum(4)).par.reduceCommutativeM)(equalTo(Some(Sum(10))))
      },
      test("reduceCommutative: empty returns None") {
        assert(List[Sum[Int]]().par.reduceCommutative)(equalTo(None))
      },
      test("reduceCommutativeIdentity: non-empty returns a value") {
        assert(List(Sum(1), Sum(2), Sum(3), Sum(4)).par.reduceCommutativeIdentity)(equalTo(Sum(10)))
      },
      testM("reduceCommutativeIdentityM: empty returns the `identity` element, non-blocking") {
        assertM(List[Sum[Int]]().par.reduceCommutativeIdentityM)(equalTo(Sum(0)))
      }
    )
}
