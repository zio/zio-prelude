package zio.prelude
package experimental

trait BothCompose[:=>[-_, +_], :*:[+_, +_]] extends AssociativeCompose[:=>] {
  def fromFirst[A]: (A :*: Any) :=> A
  def fromSecond[B]: (Any :*: B) :=> B
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

object BothCompose {

  implicit val FunctionBothEitherIdentityCompose
    : ApplicationCompose[Function, ({ type lambda[+f, +s] = (f, s) })#lambda, Function] =
    new ApplicationCompose[Function, ({ type lambda[+f, +s] = (f, s) })#lambda, Function] {

      override def compose[A, B, C](bc: B => C, ab: A => B): A => C =
        bc.compose(ab)

      override def fromFirst[A]: Function[(A, Any), A] = _._1

      override def fromSecond[B]: Function[(Any, B), B] = _._2

      override def toBoth[A, B, C](a2b: Function[A, B])(a2c: Function[A, C]): Function[A, (B, C)] = { a =>
        (a2b(a), a2c(a))
      }

      override def application[A, B]: Function[(Function[A, B], A), B] = { case (a2b, a) =>
        a2b(a)
      }

      override def curry[A, B, C](f: Function[(A, B), C]): Function[A, Function[B, C]] = { a => b =>
        f((a, b))
      }

      override def uncurry[A, B, C](g: Function[A, Function[B, C]]): Function[(A, B), C] = { case (a, b) =>
        g(a)(b)
      }

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
