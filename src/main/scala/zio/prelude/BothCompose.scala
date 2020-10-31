package zio.prelude

trait BothCompose[:=>[-_, +_], :*:[+_, +_]] extends AssociativeCompose[:=>] {
  def fromFirst[A, B]: (A :*: B) :=> A
  def fromSecond[A, B]: (A :*: B) :=> B
  def toBoth[A, B, C](a2b: A :=> B)(a2c: A :=> C): A :=> (B :*: C)

  def bothCompose[A, B, C](
    a2b: A :=> B,
    a2c: A :=> C,
    a2bc: A :=> (B :*: C)
  )(implicit eqA2B: Equal[A :=> B], eqA2C: Equal[A :=> C], eqA2BC: Equal[A :=> (B :*: C)]): Boolean = {
    val law1 = compose[A, B :*: C, B](fromFirst, toBoth(a2b)(a2c)) === a2b
    val law2 = compose[A, B :*: C, C](fromSecond, toBoth(a2b)(a2c)) === a2c
    val law3 = toBoth(compose[A, B :*: C, B](fromFirst, a2bc))(compose[A, B :*: C, C](fromSecond, a2bc)) === a2bc

    law1 && law2 && law3
  }
}

trait BothComposeSyntax {
  implicit class BothComposeOps[A, B, :=>[-_, +_]](private val a2b: A :=> B) {

    /** A symbolic alias for `toBoth`. Composes `A -> B` with `A -> C` to form `A -> (B, C)`. */
    def &&&[C, :*:[+_, +_]](implicit both: BothCompose[:=>, :*:]): (A :=> C) => (A :=> (B :*: C)) =
      both.toBoth(a2b)

    /** Composes `A -> B` with `A -> C` to form `A -> (B, C)`. */
    def toBoth[C, :*:[+_, +_]](implicit both: BothCompose[:=>, :*:]): (A :=> C) => (A :=> (B :*: C)) =
      both.toBoth(a2b)
  }
}
