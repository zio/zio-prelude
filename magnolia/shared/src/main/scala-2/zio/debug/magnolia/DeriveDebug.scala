package zio.debug.magnolia

import magnolia1._
import zio.prelude.Debug
import zio.prelude.Debug.Repr

import scala.collection.immutable.ListMap

object DeriveDebug {

  type Typeclass[T] = Debug[T]

  def join[T](ctx: CaseClass[Debug, T]): Debug[T] =
    if (ctx.isValueClass) { (a: T) =>
      Repr.VConstructor(
        ctx.typeName.owner.split('.').toList,
        ctx.typeName.short,
        ctx.parameters.map(p => p.typeclass.debug(p.dereference(a))).toList
      )
    } else if (ctx.isObject) { (_: T) =>
      Repr.Object(ctx.typeName.owner.split('.').toList, ctx.typeName.short)
    } else { (a: T) =>
      Repr.Constructor(
        ctx.typeName.owner.split('.').toList,
        ctx.typeName.short,
        ListMap.apply(ctx.parameters.map(p => p.label -> p.typeclass.debug(p.dereference(a))): _*)
      )
    }

  def split[T](ctx: SealedTrait[Debug, T]): Debug[T] =
    new Debug[T] { self =>
      def debug(a: T): Repr = ctx.split(a) { sub =>
        sub.typeclass.debug(sub.cast(a))
      }
    }

  def derived[T]: Debug[T] = macro Magnolia.gen[T]
}
