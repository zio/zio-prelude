object Example extends App {

  import zio.prelude.Equal

  1 === 1

  val laws = Equal.laws

  val intEqual = Equal.instance[Int]
}
