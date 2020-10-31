package zio.prelude

trait EitherCompose[:=>[-_, +_], :+:[+_, +_]] extends AssociativeCompose[:=>] {
  def toLeft[A, B]: A :=> (A :+: B)
  def toRight[A, B]: B :=> (A :+: B)
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

trait EitherComposeSyntax {
  implicit class EitherComposeOps[A, C, :=>[-_, +_]](private val a2b: A :=> C) {

    /** A symbolic alias for `fromEither`. Composes `A -> C` with `B -> C` to form `A or B -> C`. */
    def |||[B, :+:[+_, +_]](implicit either: EitherCompose[:=>, :+:]): (=> B :=> C) => ((A :+: B) :=> C) =
      either.fromEither(a2b)

    /** Composes `A -> C` with `B -> C` to form `A or B -> C`. */
    def fromEither[B, :+:[+_, +_]](implicit either: EitherCompose[:=>, :+:]): (=> B :=> C) => ((A :+: B) :=> C) =
      either.fromEither(a2b)
  }
}
