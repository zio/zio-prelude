package zio.prelude
package experimental

trait ApplicationCompose[:=>[-_, +_]] extends BothCompose[:=>] {

  type :-->[-_, +_]

  def application[A, B]: ((A :--> B) :*: A) :=> B
  def curry[A, B, C](f: (A :*: B) :=> C): A :=> (B :--> C)
  def uncurry[A, B, C](g: A :=> (B :--> C)): (A :*: B) :=> C

  def implyCompose[A, B, C](
    f: (A :*: B) :=> C,
    g: A :=> (B :--> C)
  )(implicit eqF: Equal[(A :*: B) :=> C], eqG: Equal[A :=> (B :--> C)]): Boolean = {
    val law1 = uncurry(curry(f)) === f
    val law2 = curry(uncurry(g)) === g
    val law3 = compose[A :*: B, (B :--> C) :*: B, C](
      application[B, C],
      toBoth(compose[A :*: B, A, B :--> C](curry(f), fromFirst))(fromSecond)
    ) === f

    law1 && law2 && law3
  }
}

object ApplicationCompose {
  type Aux[:=>[-_, +_], Product[+_, +_], Arrow[-_, +_]] = ApplicationCompose[:=>] {
    type :*:[+f, +s]  = Product[f, s]
    type :-->[-t, +r] = Arrow[t, r]
  }
}

trait ApplicationComposeSyntax {

  implicit class ApplicationComposeCurryOps[A, B, C, :=>[-_, +_], :*:[+_, +_], :-->[-_, +_]](
    private val ab2c: (A :*: B) :=> C
  ) {

    /** Curries `(A, B) -> C` to `A -> (B -> C)`. */
    def curry(implicit applicationCompose: ApplicationCompose.Aux[:=>, :*:, :-->]): A :=> (B :--> C) =
      applicationCompose.curry(ab2c)
  }

  implicit class ApplicationComposeUncurryOps[A, B, C, :=>[-_, +_], :*:[+_, +_], :-->[-_, +_]](
    private val a2b2c: A :=> (B :--> C)
  ) {

    /** Uncurries `A -> (B -> C)` to `(A, B) -> C`. */
    def uncurry(implicit applicationCompose: ApplicationCompose.Aux[:=>, :*:, :-->]): (A :*: B) :=> C =
      applicationCompose.uncurry(a2b2c)
  }
}
