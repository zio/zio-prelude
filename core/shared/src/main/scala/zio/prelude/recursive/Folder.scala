package zio.prelude.recursive

import zio.prelude._

/**
 * A reusable function from one layer of type `Case[A]`, whose sub-structures have already been folded to `A`, to an
 * `A`. Pass it to `Recursive#fold` to collapse a whole [[Recursive]] structure.
 *
 * {{{
 * import zio.prelude._
 *
 * val toInt: Folder[Option, Int] =
 *   Folder[Option, Int] {
 *     case None    => 0
 *     case Some(n) => n + 1
 *   }
 *
 * val three: Recursive[Option, Any] =
 *   Recursive.unfold[Option, Int](3)(n => if (n == 0) None else Some(n - 1))
 *
 * val result: Int = three.fold(toInt)
 * // 3
 * }}}
 */
sealed trait Folder[Case[+_], A] extends (Case[A] => A) { self =>

  /**
   * Combines this folder with `that` into one folder that computes both results in a single pass and returns them as a
   * pair.
   */
  def zip[B](that: Folder[Case, B])(implicit covariant: Covariant[Case]): Folder[Case, (A, B)] =
    Folder(tuple => self(tuple.map(_._1)) -> that(tuple.map(_._2)))
}

/**
 * Constructors and instances for [[Folder]].
 */
object Folder {

  /**
   * Creates a folder from the function `f`.
   */
  def apply[Case[+_], A](f: Case[A] => A): Folder[Case, A] =
    new Folder[Case, A] {
      def apply(caseValue: Case[A]): A =
        f(caseValue)
    }

  /**
   * The `Invariant` instance for `Folder`, which changes the result type of a folder through an equivalence.
   */
  implicit def FolderInvariant[Case[+_]: Covariant]: Invariant[({ type lambda[x] = Folder[Case, x] })#lambda] =
    new Invariant[({ type lambda[x] = Folder[Case, x] })#lambda] {
      def invmap[A, B](f: A <=> B): Folder[Case, A] <=> Folder[Case, B] =
        Equivalence(
          folder => Folder(caseValue => f.to(folder(caseValue.map(f.from)))),
          folder => Folder(caseValue => f.from(folder(caseValue.map(f.to))))
        )
    }
}
