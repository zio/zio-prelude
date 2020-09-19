package zio.prelude

trait ZDerive2[F[_, _], TypeclassF[_], Typeclass1[_], Typeclass2[_]] {
  def derive[A: Typeclass1, B: Typeclass2]: TypeclassF[F[A, B]]
}

object ZDerive2 {

  implicit val FunctionDeriveEqualFromEnumerableEqual: ZDerive2[Function, Equal, Enumerable, Equal] =
    new ZDerive2[Function, Equal, Enumerable, Equal] {
      def derive[A: Enumerable, B: Equal]: Equal[A => B] =
        Equal.make { (f, g) =>
          Enumerable[A].enumerate.forall { a =>
            f(a) === g(a)
          }
        }
    }
}