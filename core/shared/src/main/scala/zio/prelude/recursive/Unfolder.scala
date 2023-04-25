package zio.prelude.recursive

import zio.prelude._

sealed trait Unfolder[Case[+_], A] extends (A => Case[A]) { self =>

  def zip[B](that: Unfolder[Case, B])(implicit both: AssociativeBoth[Case]): Unfolder[Case, (A, B)] =
    Unfolder { case (a, b) => self(a).zip(that(b)) }
}

object Unfolder {

  def apply[Case[+_], A](f: A => Case[A]): Unfolder[Case, A] =
    new Unfolder[Case, A] {
      def apply(a: A): Case[A] =
        f(a)
    }

  implicit def UnfolderInvariant[Case[+_]: Covariant]: Invariant[({ type lambda[x] = Unfolder[Case, x] })#lambda] =
    new Invariant[({ type lambda[x] = Unfolder[Case, x] })#lambda] {
      def invmap[A, B](f: A <=> B): Unfolder[Case, A] <=> Unfolder[Case, B] =
        Equivalence(
          unfolder => Unfolder(b => unfolder(f.from(b)).map(f.to)),
          unfolder => Unfolder(a => unfolder(f.to(a)).map(f.from))
        )
    }
}
