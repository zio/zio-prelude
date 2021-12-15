package zio.prelude.recursive

import zio.prelude._

final case class Recursive[Case[+_]](caseValue: Case[Recursive[Case]]) {

  def fold[Z](f: Case[Z] => Z)(implicit covariant: Covariant[Case]): Z =
    f(caseValue.map(_.fold(f)))

  /**
   * Folds over the recursive data structure to reduce it to a summary value,
   * providing access to the recursive structure annotated with the current
   * previous summary values in each step of the fold.
   */
  def foldAnnotated[Z](f: Case[Annotated[Case, Z]] => Z)(implicit covariant: Covariant[Case]): Z = {
    def annotate(recursive: Recursive[Case]): Annotated[Case, Z] =
      Annotated(recursive.caseValue.map(annotate), recursive.foldAnnotated(f))
    f(caseValue.map(annotate))
  }

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: Case[Z] => F[Z])(implicit
    foreach: ForEach[Case]
  ): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))

  def foldRecursive[Z](f: Case[(Recursive[Case], Z)] => Z)(implicit covariant: Covariant[Case]): Z =
    f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))
}

object Recursive {

  def unfold[Case[+_], Z](z: Z)(f: Z => Case[Z])(implicit covariant: Covariant[Case]): Recursive[Case] =
    Recursive(f(z).map(unfold(_)(f)))

  def unfoldRecursive[Case[+_], Z](z: Z)(
    f: Either[Recursive[Case], Z] => Case[Either[Recursive[Case], Z]]
  )(implicit covariant: Covariant[Case]): Recursive[Case] =
    Recursive {
      f(Right(z)).map {
        case Left(recursive) => recursive
        case Right(z)        => unfoldRecursive(z)(f)
      }
    }
}
