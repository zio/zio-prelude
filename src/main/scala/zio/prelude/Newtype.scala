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

  type Newtype[A] = instance.Newtype[A]

  def newtype[A]: Newtype[A] = instance.newtype[A]

  def derive[F[_]]: NewtypeDerive[F] = new NewtypeDerive[F]

  class NewtypeDerive[F[_]] {
    def forNewtype[A](n: NewtypeModule.instance.Newtype[A])(implicit instance: F[A]): F[n.Type] = n.wrapAll(instance)
  }
}
