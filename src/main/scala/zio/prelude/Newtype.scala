package zio.prelude

sealed trait NewtypeModule {
  def newtype[A]: Newtype[A]

  private type Id[A] = A

  sealed trait Newtype[A] {
    type Type <: A

    def apply(value: A): Type = wrap(value)

    def unapply(value: Type): Some[A] = unwrap(value)

    def wrap(value: A): Type = wrapAll[Id](value)

    def unwrap(value: Type): Some[A] = Some(unwrapAll[Id](value))

    def wrapAll[F[_]](value: F[A]): F[Type]

    def unwrapAll[F[_]](value: F[Type]): F[A]
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
}
