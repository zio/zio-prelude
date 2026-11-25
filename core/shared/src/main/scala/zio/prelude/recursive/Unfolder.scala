package zio.prelude.recursive

import zio.prelude._

/**
 * A reusable function from a seed of type `A` to one layer of type `Case[A]`, whose sub-structures are the seeds for
 * the next step. Pass it to `Recursive.unfold` to build a whole [[Recursive]] structure from a seed.
 *
 * {{{
 * import zio.prelude._
 *
 * val countDown: Unfolder[Option, Int] =
 *   Unfolder[Option, Int](n => if (n == 0) None else Some(n - 1))
 *
 * val three: Recursive[Option, Any] = Recursive.unfold(3)(countDown)
 * }}}
 */
sealed trait Unfolder[Case[+_], A] extends (A => Case[A]) { self =>

  /**
   * Combines this unfolder with `that` into one unfolder over pairs of seeds, which combines the two layers with the
   * `AssociativeBoth` instance of `Case`.
   */
  def zip[B](that: Unfolder[Case, B])(implicit both: AssociativeBoth[Case]): Unfolder[Case, (A, B)] =
    Unfolder { case (a, b) => self(a).zip(that(b)) }
}

/**
 * Constructors and instances for [[Unfolder]].
 */
object Unfolder {

  /**
   * Creates an unfolder from the function `f`.
   */
  def apply[Case[+_], A](f: A => Case[A]): Unfolder[Case, A] =
    new Unfolder[Case, A] {
      def apply(a: A): Case[A] =
        f(a)
    }

  /**
   * The `Invariant` instance for `Unfolder`, which changes the seed type of an unfolder through an equivalence.
   */
  implicit def UnfolderInvariant[Case[+_]: Covariant]: Invariant[({ type lambda[x] = Unfolder[Case, x] })#lambda] =
    new Invariant[({ type lambda[x] = Unfolder[Case, x] })#lambda] {
      def invmap[A, B](f: A <=> B): Unfolder[Case, A] <=> Unfolder[Case, B] =
        Equivalence(
          unfolder => Unfolder(b => unfolder(f.from(b)).map(f.to)),
          unfolder => Unfolder(a => unfolder(f.to(a)).map(f.from))
        )
    }
}
