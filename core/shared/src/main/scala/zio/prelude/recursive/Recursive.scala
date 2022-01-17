package zio.prelude.recursive

import zio._
import zio.prelude._

final case class Recursive[Case[+_], +Annotations](
  caseValue: Case[Recursive[Case, Annotations]],
  annotations: ZEnvironment[Annotations]
) {

  def fold[Z](f: Case[Z] => Z)(implicit covariant: Covariant[Case]): Z =
    f(caseValue.map(_.fold(f)))

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: Case[Z] => F[Z])(implicit
    foreach: ForEach[Case]
  ): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))

  def foldRecursive[Z](f: Case[(Recursive[Case, Annotations], Z)] => Z)(implicit covariant: Covariant[Case]): Z =
    f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))
}

object Recursive {

  def unfold[Case[+_], Z](z: Z)(f: Z => Case[Z])(implicit covariant: Covariant[Case]): Recursive[Case, Any] =
    Recursive(f(z).map(unfold(_)(f)), ZEnvironment.empty)

  def unfoldRecursive[Case[+_], Z](z: Z)(
    f: Either[Recursive[Case, Any], Z] => Case[Either[Recursive[Case, Any], Z]]
  )(implicit covariant: Covariant[Case]): Recursive[Case, Any] =
    Recursive(
      f(Right(z)).map {
        case Left(recursive) => recursive
        case Right(z)        => unfoldRecursive(z)(f)
      },
      ZEnvironment.empty
    )
}
