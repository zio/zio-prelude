package zio.prelude

import zio.test.Assertion

sealed trait NewtypeModule {
  def newtype[A]: Newtype[A]

  def newtypeSmart[A](assertion: Assertion[A]): NewtypeSmart[A]

  def subtype[A]: Subtype[A]

  def subtypeSmart[A](assertion: Assertion[A]): SubtypeSmart[A]

  private type Id[A] = A

  sealed trait Newtype[A] {
    type Type

    def apply(value: A): Type = wrap(value)

    def unapply(value: Type): Some[A] = Some(unwrap(value))

    def wrap(value: A): Type = wrapAll[Id](value)

    def unwrap(value: Type): A = unwrapAll[Id](value)

    def wrapAll[F[_]](value: F[A]): F[Type]

    def unwrapAll[F[_]](value: F[Type]): F[A]
  }

  sealed trait NewtypeSmart[A] {
    type Type

    def make(value: A): Validation[String, Type] = wrap(value)

    def unapply(value: Type): Some[A] = Some(unwrap(value))

    def wrap(value: A): Validation[String, Type]

    def unwrap(value: Type): A = unwrapAll[Id](value)

    def wrapAll(value: List[A]): Validation[String, List[Type]]

    def unwrapAll[F[_]](value: F[Type]): F[A]
  }

  sealed trait Subtype[A] extends Newtype[A] {
    type Type <: A
  }

  sealed trait SubtypeSmart[A] extends NewtypeSmart[A] {
    type Type <: A
  }
}
private[prelude] object NewtypeModule {
  val instance: NewtypeModule =
    new NewtypeModule {
      def newtype[A]: Newtype[A] =
        new Newtype[A] {
          type Type = A

          def wrapAll[F[_]](value: F[A]): F[Type] = value

          def unwrapAll[F[_]](value: F[Type]): F[A] = value
        }

      def newtypeSmart[A](assertion: Assertion[A]): NewtypeSmart[A] =
        new NewtypeSmart[A] {
          type Type = A

          def wrap(value: A): Validation[String, A] =
            Validation.fromAssert(value)(assertion)

          def wrapAll(value: List[A]): Validation[String, List[A]] =
            Validation.collectAllPar(value.map(wrap))

          def unwrapAll[F[_]](value: F[A]): F[A] = value
        }

      def subtype[A]: Subtype[A] =
        new Subtype[A] {
          type Type = A

          def wrapAll[F[_]](value: F[A]): F[Type] = value

          def unwrapAll[F[_]](value: F[Type]): F[A] = value
        }

      def subtypeSmart[A](assertion: Assertion[A]): SubtypeSmart[A] =
        new SubtypeSmart[A] {
          type Type = A

          def wrap(value: A): Validation[String, A] =
            Validation.fromAssert(value)(assertion)

          def wrapAll(value: List[A]): Validation[String, List[A]] =
            Validation.collectAllPar(value.map(wrap))

          def unwrapAll[F[_]](value: F[A]): F[A] = value
        }
    }
}
trait NewtypeExports {
  import NewtypeModule._

  abstract class Newtype[A] extends instance.Newtype[A] {
    val newtype: instance.Newtype[A] = instance.newtype[A]

    type Type = newtype.Type

    def wrapAll[F[_]](value: F[A]): F[Type] = newtype.wrapAll(value)

    def unwrapAll[F[_]](value: F[Type]): F[A] = newtype.unwrapAll(value)
  }

  abstract class NewtypeSmart[A](assertion: Assertion[A]) extends instance.NewtypeSmart[A] {
    val newtype: instance.NewtypeSmart[A] = instance.newtypeSmart[A](assertion)

    type Type = newtype.Type

    def wrap(value: A): Validation[String, Type] = newtype.wrap(value)

    def wrapAll(value: List[A]): Validation[String, List[Type]] = newtype.wrapAll(value)

    def unwrapAll[F[_]](value: F[Type]): F[A] = newtype.unwrapAll(value)
  }

  abstract class Subtype[A] extends instance.Subtype[A] {
    val subtype: instance.Subtype[A] = instance.subtype[A]

    type Type = subtype.Type

    def wrapAll[F[_]](value: F[A]): F[Type] = subtype.wrapAll(value)

    def unwrapAll[F[_]](value: F[Type]): F[A] = subtype.unwrapAll(value)
  }

  abstract class SubtypeSmart[A](assertion: Assertion[A]) extends instance.SubtypeSmart[A] {
    val subtype: instance.SubtypeSmart[A] = instance.subtypeSmart[A](assertion)

    type Type = subtype.Type

    def wrap(value: A): Validation[String, Type] = subtype.wrap(value)

    def wrapAll(value: List[A]): Validation[String, List[Type]] = subtype.wrapAll(value)

    def unwrapAll[F[_]](value: F[Type]): F[A] = subtype.unwrapAll(value)
  }
}
