package zio.debug.magnolia

import magnolia1._

import scala.collection.immutable.ListMap
import scala.language.experimental.macros
import zio.prelude.Debug
import zio.prelude.Debug.Repr

object DeriveDebug extends AutoDerivation[Debug] {

  type Typeclass[T] = Debug[T]

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

  def split[T](ctx: SealedTrait[Debug, T]): Debug[T] =
    new Debug[T] { self =>
      def debug(a: T): Repr = ctx.choose(a) { sub =>
        sub.typeclass.debug(sub.cast(a))
      }
    }

}
