package zio.prelude.recursive

import zio.prelude._

sealed trait Coalgebra[Case[+_], A] extends (A => Case[A]) { self =>

  def zip[B](that: Coalgebra[Case, B])(implicit both: AssociativeBoth[Case]): Coalgebra[Case, (A, B)] =
    Coalgebra { case (a, b) => self(a).zip(that(b)) }
}

object Coalgebra {

  def apply[Case[+_], A](f: A => Case[A]): Coalgebra[Case, A] =
    new Coalgebra[Case, A] {
      def apply(a: A): Case[A] =
        f(a)
    }

  implicit def CoalgebraInvariant[Case[+_]: Covariant]: Invariant[({ type lambda[x] = Coalgebra[Case, x] })#lambda] =
    new Invariant[({ type lambda[x] = Coalgebra[Case, x] })#lambda] {
      def invmap[A, B](f: A <=> B): Coalgebra[Case, A] <=> Coalgebra[Case, B] =
        Equivalence(
          coalgebra => Coalgebra(b => coalgebra(f.from(b)).map(f.to)),
          coalgebra => Coalgebra(a => coalgebra(f.to(a)).map(f.from))
        )
    }
}
