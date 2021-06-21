package zio.prelude

object TestSet {
  case class MySet(map: Map[Int, NewtypeSpec.Natural])

  def main(args: Array[String]): Unit =
    println(MySet(Map(10 -> NewtypeSpec.Natural(-10))))
}
