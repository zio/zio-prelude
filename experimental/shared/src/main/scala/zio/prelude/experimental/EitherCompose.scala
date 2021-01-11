package zio.prelude
package experimental

trait EitherCompose[:=>[-_, +_]] extends AssociativeCompose[:=>] {

  type :+:[+_, +_]

  def toLeft[A]: A :=> (A :+: Nothing)
  def toRight[B]: B :=> (Nothing :+: B)
  def fromEither[A, B, C](a2c: => A :=> C)(b2c: => B :=> C): (A :+: B) :=> C

  def eitherCompose[A, B, C](
    a2c: A :=> C,
    b2c: B :=> C,
    ab2c: (A :+: B) :=> C
  )(implicit eqA2C: Equal[A :=> C], eqB2C: Equal[B :=> C], eqA2BC: Equal[(A :+: B) :=> C]): Boolean = {
    val law1 = compose[A, A :+: B, C](fromEither(a2c)(b2c), toLeft) === a2c
    val law2 = compose[B, A :+: B, C](fromEither(a2c)(b2c), toRight) === b2c
    val law3 = fromEither(compose[A, A :+: B, C](ab2c, toLeft))(compose[B, A :+: B, C](ab2c, toRight)) === ab2c

    law1 && law2 && law3
  }
}

object EitherCompose {

  type Aux[:=>[-_, +_], Sum[+_, +_]] = EitherCompose[:=>] {
    type :+:[+l, +r] = Sum[l, r]
  }

  implicit val FunctionEitherCompose: EitherCompose[Function] = new EitherCompose[Function] {

    type :+:[+l, +r] = Either[l, r]

    override def compose[A, B, C](bc: B => C, ab: A => B): A => C =
      AssociativeCompose.FunctionIdentityCompose.compose(bc, ab)

    override def toLeft[A]: Function[A, Either[A, Nothing]] = Left(_)

    override def toRight[B]: Function[B, Either[Nothing, B]] = Right(_)

    override def fromEither[A, B, C](a2c: => Function[A, C])(b2c: => Function[B, C]): Function[Either[A, B], C] = {
      case Left(a)  => a2c(a)
      case Right(b) => b2c(b)
    }
  }
}

trait EitherComposeSyntax {
  implicit class EitherComposeOps[A, C, :=>[-_, +_]](private val a2b: A :=> C) {

    /** A symbolic alias for `fromEither`. Composes `A -> C` with `B -> C` to form `A or B -> C`. */
    def |||[B, :+:[+_, +_]](implicit eitherCompose: EitherCompose.Aux[:=>, :+:]): (=> B :=> C) => ((A :+: B) :=> C) =
      eitherCompose.fromEither(a2b)

    /** Composes `A -> C` with `B -> C` to form `A or B -> C`. */
    def fromEither[B, :+:[+_, +_]](implicit
      eitherCompose: EitherCompose.Aux[:=>, :+:]
    ): (=> B :=> C) => ((A :+: B) :=> C) =
      eitherCompose.fromEither(a2b)
  }
}
