package zio.prelude.recursive

import zio._
import zio.prelude._
import zio.prelude.fx._
import zio.stm._

/**
 * A recursive data structure built from layers of type `Case`, with annotations of type `Annotations` stored next to
 * each layer.
 *
 * `Case[A]` describes one layer of the structure, and `A` marks the places where that layer refers to sub-structures.
 * `Recursive` fills those places with further `Recursive` values, so the recursion does not have to be written by hand.
 * Operators such as `fold`, `foldDown` and `transformUp` then work on any structure whose layer type has a `Covariant`
 * or `ForEach` instance. The `annotations` of a layer are a `ZEnvironment`; use `ZEnvironment.empty` and `Any` when no
 * annotations are needed.
 *
 * Using `List` as the layer type gives a tree in which every node holds the list of its children:
 *
 * {{{
 * import zio._
 * import zio.prelude._
 *
 * def node(children: Recursive[List, Any]*): Recursive[List, Any] =
 *   Recursive(children.toList, ZEnvironment.empty)
 *
 * val tree: Recursive[List, Any] = node(node(node()), node())
 *
 * val size: Int  = tree.fold[Int](sizes => sizes.sum + 1)
 * val depth: Int = tree.fold[Int](depths => if (depths.isEmpty) 1 else depths.max + 1)
 * // size == 4, depth == 3
 * }}}
 *
 * @tparam Case the type constructor that describes one layer of the structure
 * @tparam Annotations the type of the services held in each layer's `annotations`
 * @param caseValue the outermost layer, whose sub-structures are `Recursive` values
 * @param annotations the annotations of the outermost layer
 * @see [[https://zio.dev/zio-prelude/functional-data-types/recursive/ Recursive]]
 */
final case class Recursive[Case[+_], +Annotations](
  caseValue: Case[Recursive[Case, Annotations]],
  annotations: ZEnvironment[Annotations]
) { self =>

  /**
   * Collapses the structure from the bottom up. `f` receives one layer whose sub-structures have already been replaced
   * by their results, and returns the result for that layer.
   *
   * {{{
   * import zio._
   * import zio.prelude._
   *
   * val three: Recursive[Option, Any] =
   *   Recursive.unfold[Option, Int](3)(n => if (n == 0) None else Some(n - 1))
   *
   * val count: Int = three.fold[Int] {
   *   case None    => 0
   *   case Some(n) => n + 1
   * }
   * // count == 3
   * }}}
   */
  def fold[Z](f: Case[Z] => Z)(implicit covariant: Covariant[Case]): Z =
    f(caseValue.map(_.fold(f)))

  /**
   * Visits every node from this node downwards in pre-order (a node before its sub-structures, and sub-structures in
   * the order of the layer's `ForEach` instance), threading the accumulator `z` through `f`.
   *
   * {{{
   * import zio._
   * import zio.prelude._
   *
   * def node(children: Recursive[List, Any]*): Recursive[List, Any] =
   *   Recursive(children.toList, ZEnvironment.empty)
   *
   * val tree: Recursive[List, Any] = node(node(node()), node())
   *
   * val childCounts: List[Int] =
   *   tree.foldDown(List.empty[Int])((acc, n) => n.caseValue.size :: acc).reverse
   * // List(2, 1, 0, 0)
   * }}}
   */
  def foldDown[Z](z: Z)(f: (Z, Recursive[Case, Annotations]) => Z)(implicit foreach: ForEach[Case]): Z =
    caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

  /**
   * Like `foldDown`, but takes a partial function, and leaves the accumulator unchanged at the nodes where `pf` is not
   * defined.
   */
  def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, Recursive[Case, Annotations]), Z])(implicit
    foreach: ForEach[Case]
  ): Z =
    foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  /**
   * Collapses the structure from the bottom up like `fold`, but `f` returns its result inside the effect `F`, so that
   * producing a result for a layer can fail or use the effect. When `F` is `Option`, the fold returns `None` when `f`
   * returns `None` for a layer.
   */
  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: Case[Z] => F[Z])(implicit
    foreach: ForEach[Case]
  ): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))

  /**
   * `foldM` specialised to `ZPure`: `f` returns a `ZPure` computation for each layer, and the state is passed from one
   * layer's computation to the next.
   */
  def foldPure[W, S, R, E, Z](f: Case[Z] => ZPure[W, S, S, R, E, Z])(implicit
    foreach: ForEach[Case]
  ): ZPure[W, S, S, R, E, Z] =
    foldM(f)

  /**
   * Collapses the structure from the bottom up like `fold`, but `f` receives each sub-result paired with the
   * sub-structure it was computed from.
   */
  def foldRecursive[Z](f: Case[(Recursive[Case, Annotations], Z)] => Z)(implicit covariant: Covariant[Case]): Z =
    f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))

  /**
   * `foldM` specialised to `ZSTM`: `f` returns a transactional effect for each layer.
   */
  def foldSTM[R, E, Z](f: Case[Z] => ZSTM[R, E, Z])(implicit foreach: ForEach[Case]): ZSTM[R, E, Z] =
    foldM(f)

  /**
   * Threads the accumulator `z` through `f` over every node, visiting this node last.
   *
   * This is a known bug: the nodes under this one are visited in the order of `foldDown` (pre-order), not in
   * post-order, because the sub-structures are folded with `foldDown`. Only this node itself is visited after its
   * sub-structures. For the tree in this example, a post-order traversal would give `List(0, 1, 0, 2)`:
   *
   * {{{
   * import zio._
   * import zio.prelude._
   *
   * def node(children: Recursive[List, Any]*): Recursive[List, Any] =
   *   Recursive(children.toList, ZEnvironment.empty)
   *
   * val tree: Recursive[List, Any] = node(node(node()), node())
   *
   * val childCounts: List[Int] =
   *   tree.foldUp(List.empty[Int])((acc, n) => n.caseValue.size :: acc).reverse
   * // List(1, 0, 0, 2)
   * }}}
   *
   * When `f` does not depend on the order of the nodes, such as when it counts them, the result is the same as with
   * `foldDown`.
   */
  def foldUp[Z](z: Z)(f: (Z, Recursive[Case, Annotations]) => Z)(implicit foreach: ForEach[Case]): Z =
    f(caseValue.foldLeft(z)((z, recursive) => recursive.foldDown(z)(f)), self)

  /**
   * Like `foldUp`, but takes a partial function, and leaves the accumulator unchanged at the nodes where `pf` is not
   * defined. It visits the nodes in the same order as `foldUp`.
   */
  def foldUpSome[Z](z: Z)(pf: PartialFunction[(Z, Recursive[Case, Annotations]), Z])(implicit
    foreach: ForEach[Case]
  ): Z =
    foldUp(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  /**
   * `foldM` specialised to `ZValidation`: `f` returns a `ZValidation` for each layer.
   */
  def foldValidation[W, E, Z](f: Case[Z] => ZValidation[W, E, Z])(implicit
    foreach: ForEach[Case]
  ): ZValidation[W, E, Z] =
    foldM(f)

  /**
   * `foldM` specialised to `ZIO`: `f` returns a `ZIO` effect for each layer.
   */
  def foldZIO[R, E, Z](f: Case[Z] => ZIO[R, E, Z])(implicit foreach: ForEach[Case]): ZIO[R, E, Z] =
    foldM(f)

  /**
   * Rebuilds the structure from the top down: applies `f` to this node, then transforms the sub-structures of the node
   * that `f` returns in the same way.
   *
   * {{{
   * import zio._
   * import zio.prelude._
   *
   * def node(children: Recursive[List, Any]*): Recursive[List, Any] =
   *   Recursive(children.toList, ZEnvironment.empty)
   *
   * val tree: Recursive[List, Any] = node(node(node()), node())
   *
   * def dropLeaves(n: Recursive[List, Any]): Recursive[List, Any] =
   *   Recursive(n.caseValue.filter(_.caseValue.nonEmpty), n.annotations)
   *
   * val size: Int = tree.transformDown[Any](dropLeaves).fold[Int](_.sum + 1)
   * // 2: the root keeps its inner child, which then loses its own leaf
   * }}}
   *
   * This is a known bug: every node of the result gets the annotations of this (the root) node. `f` receives each node
   * with its own annotations, but the annotations of the node that `f` returns are replaced by the root's, so even
   * `transformDown(identity)` replaces the annotations of every node under the root.
   */
  def transformDown[Annotations0 >: Annotations](
    f: Recursive[Case, Annotations0] => Recursive[Case, Annotations0]
  )(implicit covariant: Covariant[Case]): Recursive[Case, Annotations0] = {
    def loop(recursive: Recursive[Case, Annotations0]): Recursive[Case, Annotations] =
      Recursive(f(recursive).caseValue.map(loop), annotations)
    loop(self)
  }

  /**
   * Like `transformDown`, but takes a partial function, and leaves the nodes where `pf` is not defined as they are
   * (apart from their annotations, which `transformDown` replaces with the root's).
   */
  def transformDownSome[Annotations0 >: Annotations](
    pf: PartialFunction[Recursive[Case, Annotations0], Recursive[Case, Annotations0]]
  )(implicit
    covariant: Covariant[Case]
  ): Recursive[Case, Annotations0] =
    transformDown[Annotations0]((recursive => pf.lift(recursive).getOrElse(recursive)))

  /**
   * Rebuilds the structure from the bottom up: transforms the sub-structures of each node first, then applies `f` to
   * the node rebuilt from the transformed sub-structures.
   *
   * {{{
   * import zio._
   * import zio.prelude._
   *
   * def node(children: Recursive[List, Any]*): Recursive[List, Any] =
   *   Recursive(children.toList, ZEnvironment.empty)
   *
   * val tree: Recursive[List, Any] = node(node(node()), node())
   *
   * def dropLeaves(n: Recursive[List, Any]): Recursive[List, Any] =
   *   Recursive(n.caseValue.filter(_.caseValue.nonEmpty), n.annotations)
   *
   * val size: Int = tree.transformUp[Any](dropLeaves).fold[Int](_.sum + 1)
   * // 1: the inner child becomes a leaf first, so the root drops it too
   * }}}
   *
   * This is a known bug: each node is rebuilt with the annotations of this (the root) node before `f` sees it, so `f`
   * never sees the annotations of the nodes under the root, and `transformUp(identity)` replaces the annotations of
   * every node under the root. The annotations of the node that `f` returns are kept.
   */
  def transformUp[Annotations0 >: Annotations](
    f: Recursive[Case, Annotations0] => Recursive[Case, Annotations0]
  )(implicit covariant: Covariant[Case]): Recursive[Case, Annotations0] = {
    def loop(recursive: Recursive[Case, Annotations0]): Recursive[Case, Annotations0] =
      f(Recursive(recursive.caseValue.map(loop), annotations))
    loop(self)
  }

  /**
   * Like `transformUp`, but takes a partial function, and leaves the nodes where `pf` is not defined as they are (apart
   * from their annotations, which `transformUp` replaces with the root's).
   */
  def transformUpSome[Annotations0 >: Annotations](
    pf: PartialFunction[Recursive[Case, Annotations0], Recursive[Case, Annotations0]]
  )(implicit
    covariant: Covariant[Case]
  ): Recursive[Case, Annotations0] =
    transformUp[Annotations0]((recursive => pf.lift(recursive).getOrElse(recursive)))
}

/**
 * Constructors for [[Recursive]] values.
 */
object Recursive {

  /**
   * Builds a structure from the seed `z`: `f` turns a seed into one layer whose sub-structures are new seeds, and
   * `unfold` repeats that step on each new seed until `f` returns layers with no seeds. The result has no annotations.
   *
   * {{{
   * import zio.prelude._
   *
   * val three: Recursive[Option, Any] =
   *   Recursive.unfold[Option, Int](3)(n => if (n == 0) None else Some(n - 1))
   * // Some(Some(Some(None))), as nested Recursive values
   * }}}
   */
  def unfold[Case[+_], Z](z: Z)(f: Z => Case[Z])(implicit covariant: Covariant[Case]): Recursive[Case, Any] =
    Recursive(f(z).map(unfold(_)(f)), ZEnvironment.empty)

  /**
   * Like `unfold`, but in each layer that `f` returns, a sub-structure is either `Right(seed)`, which is unfolded
   * further, or `Left(recursive)`, an existing structure that is used as is. `f` is called with `Right(seed)` only.
   * The layers that `f` builds have no annotations.
   */
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
