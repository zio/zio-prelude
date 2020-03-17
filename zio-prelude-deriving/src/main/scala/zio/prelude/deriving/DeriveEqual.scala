package zio.prelude.deriving

import magnolia._

import zio.prelude.Equal

object DeriveEqual {

  type Typeclass[A] = Equal[A]

  def combine[A](caseClass: CaseClass[Equal, A]): Equal[A] =
    Equal { (a1, a2) =>
      caseClass.parameters.forall { parameter =>
        parameter.typeclass.equal(parameter.dereference(a1), parameter.dereference(a2))
      }
    }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] =
    Equal { (a1, a2) =>
      sealedTrait.dispatch(a1) { subtype =>
        subtype.cast.isDefinedAt(a2) && subtype.typeclass.equal(subtype.cast(a1), subtype.cast(a2))
      }
    }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}
