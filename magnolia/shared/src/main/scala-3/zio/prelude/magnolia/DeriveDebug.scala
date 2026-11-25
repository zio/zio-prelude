package zio.prelude.magnolia

import magnolia1._

import scala.collection.immutable.ListMap
import scala.language.experimental.macros
import zio.prelude.Debug
import zio.prelude.Debug.Repr
import scala.deriving.Mirror

extension ($: Debug.type) {

  /**
   * Derives a `Debug[T]` with `DeriveDebug`. This is the `Debug.derived` that
   * a `derives Debug` clause calls; it is in scope after
   * `import zio.prelude.magnolia._`.
   */
  inline def derived[T](using Mirror.Of[T]): Debug[T] = DeriveDebug.derived[T]
}
/**
 * Derives `Debug` instances with Magnolia. A case class renders as its name
 * followed by `name = value` pairs, a case object as its name, and a sealed
 * trait as the subtype the value belongs to. On Scala 2, a value class
 * (`extends AnyVal`) renders as its name and the wrapped value; on Scala 3,
 * deriving a value class does not compile, because the compiler provides no
 * `Mirror` for it.
 *
 * Each field type needs a `Debug` instance in implicit scope, or the
 * derivation does not compile. The subtypes of a sealed trait that have no
 * instance of their own are derived along with it.
 *
 * When the derived instance of a sealed trait is itself in implicit scope,
 * rendering a value with it can throw a `StackOverflowError`. It does with an
 * `implicit val` in the trait's companion on Scala 2, and with `derives Debug`
 * on the trait or on an enum on Scala 3. Stored in a plain `val`, the
 * instance renders values.
 *
 * On Scala 3, `DeriveDebug` also has Magnolia's `autoDerived` given: after
 * `import DeriveDebug.autoDerived`, the compiler derives a missing `Debug`
 * instance for a case class implicitly.
 *
 * On Scala 2:
 *
 * {{{
 * // scala-2-only
 * import zio.prelude._
 *
 * final case class Point(x: Int, y: Int)
 * object Point {
 *   implicit val debug: Debug[Point] = DeriveDebug.derived[Point]
 * }
 *
 * Point(1, 2).render
 * // Point(x = 1, y = 2)
 * }}}
 *
 * On Scala 3:
 *
 * {{{
 * // scala-3-only
 * import zio.prelude._
 *
 * final case class Point(x: Int, y: Int) derives Debug
 *
 * Point(1, 2).render
 * // Point(x = 1, y = 2)
 * }}}
 */
object DeriveDebug extends AutoDerivation[Debug] {

  /** The type class that Magnolia derives. */
  type Typeclass[T] = Debug[T]

  /**
   * Builds the `Debug` instance of a case class, case object or value class
   * from the instances of its fields. Magnolia calls it during derivation.
   */
  def join[T](ctx: CaseClass[Debug, T]): Debug[T] =
    if (ctx.isValueClass) { (a: T) =>
      Repr.VConstructor(
        ctx.typeInfo.owner.split('.').toList,
        ctx.typeInfo.short,
        ctx.parameters.map(p => p.typeclass.debug(p.deref(a))).toList
      )
    } else if (ctx.isObject) { (_: T) =>
      Repr.Object(ctx.typeInfo.owner.split('.').toList, ctx.typeInfo.short)
    } else { (a: T) =>
      Repr.Constructor(
        ctx.typeInfo.owner.split('.').toList,
        ctx.typeInfo.short,
        ListMap.from(ctx.parameters.map(p => p.label -> p.typeclass.debug(p.deref(a))))
      )
    }

  /**
   * Builds the `Debug` instance of a sealed trait, which renders a value with
   * the instance of its subtype. Magnolia calls it during derivation.
   */
  def split[T](ctx: SealedTrait[Debug, T]): Debug[T] =
    new Debug[T] { self =>
      def debug(a: T): Repr = ctx.choose(a) { sub =>
        sub.typeclass.debug(sub.cast(a))
      }
    }

}
