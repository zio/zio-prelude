package zio.prelude

import zio.test._
import zio.test.Assertion._

object StateSpec extends DefaultRunnableSpec {

  def spec = suite("StateSpec")(
    test("map stack safety") {
      val state = (1 to 100000).foldLeft(State.succeed[Int, Int](0)) { (s, n) =>
        s.map(_ + n)
      }
      assert(state.run(0))(anything)
    },
    test("left flatMap stack safety") {
      val state = (1 to 100000).map(n => State[Int, Int](s => (s + n, n))).foldLeft(State.succeed[Int, Int](0)) { (s, n) =>
        for {
          x <- s
          y <- n
        } yield (x + y)
      }
      assert(state.run(0))(anything)
    },
    test("right flatMap stack safety") {
      val state = (1 to 100000).map(n => State[Int, Int](s => (s + n, n))).foldRight(State.succeed[Int, Int](0)) { (s, n) =>
        for {
          x <- s
          y <- n
        } yield (x + y)
      }
      assert(state.run(0))(anything)
    }
  )
}
