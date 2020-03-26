package zio.prelude.coherent

import zio.prelude._
import zio.test._

object CoherentSpec extends DefaultRunnableSpec {

  def spec = suite("CoherentSpec")(
    test("intersections can be derived") {
      val instance = implicitly[Hash[Int] with Ord[Int]]
      assert(instance.hash(0))(equalTo(0)) &&
      assert(instance.compare(0, 1))(equalTo(Ordering.LessThan))
    }
  )
}
