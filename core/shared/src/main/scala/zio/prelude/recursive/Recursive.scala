package zio.prelude.recursive

import zio._
import zio.prelude._
import zio.prelude.fx._
import zio.stm._

final case class Recursive[Case[+_]](caseValue: Case[Recursive[Case]]) { self =>

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

  def foldDown[Z](z: Z)(f: (Z, Recursive[Case]) => Z)(implicit foreach: ForEach[Case]): Z =
    caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

  def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, Recursive[Case]), Z])(implicit foreach: ForEach[Case]): Z =
    foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: Case[Z] => F[Z])(implicit
    foreach: ForEach[Case]
  ): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))

  def foldManaged[R, E, Z](f: Case[Z] => ZManaged[R, E, Z])(implicit foreach: ForEach[Case]): ZManaged[R, E, Z] =
    foldM(f)

  def foldPure[W, S, R, E, Z](f: Case[Z] => ZPure[W, S, S, R, E, Z])(implicit
    foreach: ForEach[Case]
  ): ZPure[W, S, S, R, E, Z] =
    foldM(f)

  def foldRecursive[Z](f: Case[(Recursive[Case], Z)] => Z)(implicit covariant: Covariant[Case]): Z =
    f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))

  def foldSTM[R, E, Z](f: Case[Z] => ZSTM[R, E, Z])(implicit foreach: ForEach[Case]): ZSTM[R, E, Z] =
    foldM(f)

  def foldUp[Z](z: Z)(f: (Z, Recursive[Case]) => Z)(implicit foreach: ForEach[Case]): Z =
    f(caseValue.foldLeft(z)((z, recursive) => recursive.foldDown(z)(f)), self)

  def foldUpSome[Z](z: Z)(pf: PartialFunction[(Z, Recursive[Case]), Z])(implicit foreach: ForEach[Case]): Z =
    foldUp(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  def foldValidation[W, E, Z](f: Case[Z] => ZValidation[W, E, Z])(implicit
    foreach: ForEach[Case]
  ): ZValidation[W, E, Z] =
    foldM(f)

  def foldZIO[R, E, Z](f: Case[Z] => ZIO[R, E, Z])(implicit foreach: ForEach[Case]): ZIO[R, E, Z] =
    foldM(f)

  def transformDown(f: Recursive[Case] => Recursive[Case])(implicit covariant: Covariant[Case]): Recursive[Case] = {
    def loop(recursive: Recursive[Case]): Recursive[Case] =
      Recursive(f(recursive).caseValue.map(loop))
    loop(self)
  }

  def transformDownSome(pf: PartialFunction[Recursive[Case], Recursive[Case]])(implicit
    covariant: Covariant[Case]
  ): Recursive[Case] =
    transformDown((recursive => pf.lift(recursive).getOrElse(recursive)))

  def transformUp(f: Recursive[Case] => Recursive[Case])(implicit covariant: Covariant[Case]): Recursive[Case] = {
    def loop(recursive: Recursive[Case]): Recursive[Case] =
      f(Recursive(recursive.caseValue.map(loop)))
    loop(self)
  }

  def transformUpSome(pf: PartialFunction[Recursive[Case], Recursive[Case]])(implicit
    covariant: Covariant[Case]
  ): Recursive[Case] =
    transformUp((recursive => pf.lift(recursive).getOrElse(recursive)))
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
