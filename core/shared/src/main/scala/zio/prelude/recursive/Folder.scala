package zio.prelude.recursive

import zio.prelude._

sealed trait Folder[Case[+_], A] extends (Case[A] => A) { self =>

  def zip[B](that: Folder[Case, B])(implicit covariant: Covariant[Case]): Folder[Case, (A, B)] =
    Folder(tuple => self(tuple.map(_._1)) -> that(tuple.map(_._2)))
}

object Folder {

  def apply[Case[+_], A](f: Case[A] => A): Folder[Case, A] =
    new Folder[Case, A] {
      def apply(caseValue: Case[A]): A =
        f(caseValue)
    }

  implicit def FolderInvariant[Case[+_]: Covariant]: Invariant[({ type lambda[x] = Folder[Case, x] })#lambda] =
    new Invariant[({ type lambda[x] = Folder[Case, x] })#lambda] {
      def invmap[A, B](f: A <=> B): Folder[Case, A] <=> Folder[Case, B] =
        Equivalence(
          folder => Folder(caseValue => f.to(folder(caseValue.map(f.from)))),
          folder => Folder(caseValue => f.from(folder(caseValue.map(f.to))))
        )
    }
}
