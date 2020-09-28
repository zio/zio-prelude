package zio.prelude

trait Enumerable[+A] {
  def enumerate: Iterable[A]
}

object Enumerable {

  def apply[A](implicit enumerable: Enumerable[A]): Enumerable[A] =
    enumerable

  def BooleanEnumerable: Enumerable[Boolean] =
    new Enumerable[Boolean] {
      def enumerate: Iterable[Boolean] =
        Iterable(false, true)
    }
}
