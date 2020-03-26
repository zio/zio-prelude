package zio.prelude

sealed trait NewtypeModuleF {
  def newtypeF: NewtypeF

  def subtypeF: SubtypeF

  private type Id[A] = A

  sealed trait NewtypeF {
    type Type[A]

    def apply[A](value: A): Type[A] = wrap(value)

    def unapply[A](value: Type[A]): Some[A] = Some(unwrap(value))

    def wrap[A](value: A): Type[A] = wrapAll[Id, A](value)

    def unwrap[A](value: Type[A]): A = unwrapAll[Id, A](value)

    def wrapAll[F[_], A](value: F[A]): F[Type[A]]

    def unwrapAll[F[_], A](value: F[Type[A]]): F[A]
  }

  sealed trait SubtypeF extends NewtypeF {
    type Type[A] <: A
  }
}
private[prelude] object NewtypeModuleF {
  val instance: NewtypeModuleF =
    new NewtypeModuleF {
      def newtypeF: NewtypeF =
        new NewtypeF {
          type Type[A] = A

          def wrapAll[F[_], A](value: F[A]): F[Type[A]] = value

          def unwrapAll[F[_], A](value: F[Type[A]]): F[A] = value
        }

      def subtypeF: SubtypeF =
        new SubtypeF {
          type Type[A] = A

          def wrapAll[F[_], A](value: F[A]): F[Type[A]] = value

          def unwrapAll[F[_], A](value: F[Type[A]]): F[A] = value
        }
    }
}
trait NewtypeFExports {
  import NewtypeModuleF._

  abstract class NewtypeF extends instance.NewtypeF {
    val newtypeF: instance.NewtypeF =
      instance.newtypeF

    type Type[A] = newtypeF.Type[A]

    def wrapAll[F[_], A](value: F[A]): F[Type[A]] =
      newtypeF.wrapAll(value)

    def unwrapAll[F[_], A](value: F[Type[A]]): F[A] =
      newtypeF.unwrapAll(value)
  }

  abstract class SubtypeF extends instance.SubtypeF {
    val subtypeF: instance.SubtypeF =
      instance.subtypeF

    type Type[A] = subtypeF.Type[A]

    def wrapAll[F[_], A](value: F[A]): F[Type[A]] =
      subtypeF.wrapAll(value)

    def unwrapAll[F[_], A](value: F[Type[A]]): F[A] =
      subtypeF.unwrapAll(value)
  }
}
