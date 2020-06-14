package zio.prelude

import zio.test._

object TraversableSpec extends DefaultRunnableSpec {

  def spec = suite("TraversableSpec")(
    test("zipWithIndex is stacks safe") {
      val as       = (1 to 100000).toList
      val expected = as.zipWithIndex
      val actual   = Traversable.ListTraversable.zipWithIndex(as)
      assert(actual)(equalTo(expected))
    }
  )
}
