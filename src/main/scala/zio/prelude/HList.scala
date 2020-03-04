package zio.prelude

sealed trait HList {

  def :*:[A](head: A): HList
}

object HList {

  type Empty = Empty.type

  type :*:[A, B <: HList] = Cons[A, B]

  case object Empty extends HList {
    override def :*:[A](head: A): A :*: Empty =
      Cons(head, Empty)
  }

  final case class Cons[A, B <: HList](head: A, tail: B) extends HList { self =>
    override def :*:[C](head: C): C :*: A :*: B =
      Cons(head, self)
  }
}
