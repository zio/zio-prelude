package zio.prelude.recursive

import zio.prelude._

final case class Recursive[Case[+_]](caseValue: Case[Recursive[Case]]) {

  def fold[Z](f: Case[Z] => Z)(implicit covariant: Covariant[Case]): Z =
    f(caseValue.map(_.fold(f)))

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: Case[Z] => F[Z])(implicit
    foreach: ForEach[Case]
  ): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))
}

object Recursive {

  def unfold[Case[+_], Z](z: Z)(f: Z => Case[Z])(implicit covariant: Covariant[Case]): Recursive[Case] =
    Recursive(f(z).map(unfold(_)(f)))
}
