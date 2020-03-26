package zio.prelude

object Example extends App {

  val instance = implicitly[Hash[Int] with Ord[Int]]

  println(instance.hash(1))
  println(instance.compare(1, 2))
}
