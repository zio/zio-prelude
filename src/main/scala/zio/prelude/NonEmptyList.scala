package zio.prelude

final case class NonEmptyList[+A](head: A, tail: Option[NonEmptyList[A]]) {
  def reduce[A1 >: A](f: (A1, A1) => A1): A1 = tail.map(_.reduce(f)).map(f(head, _)).getOrElse(head)
}
