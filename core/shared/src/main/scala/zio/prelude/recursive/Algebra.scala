package zio.prelude.recursive

import zio.prelude._

sealed trait Algebra[Case[+_], A] extends (Case[A] => A) { self =>

  def zip[B](that: Algebra[Case, B])(implicit covariant: Covariant[Case]): Algebra[Case, (A, B)] =
    Algebra(tuple => self(tuple.map(_._1)) -> that(tuple.map(_._2)))
}

object Algebra {

  def apply[Case[+_], A](f: Case[A] => A): Algebra[Case, A] =
    new Algebra[Case, A] {
      def apply(caseValue: Case[A]): A =
        f(caseValue)
    }

  implicit def AlgebraInvariant[Case[+_]: Covariant]: Invariant[({ type lambda[x] = Algebra[Case, x] })#lambda] =
    new Invariant[({ type lambda[x] = Algebra[Case, x] })#lambda] {
      def invmap[A, B](f: A <=> B): Algebra[Case, A] <=> Algebra[Case, B] =
        Equivalence(
          algebra => Algebra(caseValue => f.to(algebra(caseValue.map(f.from)))),
          algebra => Algebra(caseValue => f.from(algebra(caseValue.map(f.to))))
        )
    }
}
