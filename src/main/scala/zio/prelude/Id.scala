package zio.prelude

trait IdExports {

  type Id[+A] = Id.Type[A]

  object Id extends NewtypeF {

    /**
     * The `CommutativeBoth` instance for `Id`.
     */
    implicit val IdCommutativeBoth: CommutativeBoth[Id] =
      new CommutativeBoth[Id] {
        def both[A, B](fa: => Id[A], fb: => Id[B]): Id[(A, B)] = Id((Id.unwrap(fa), Id.unwrap(fb)))
      }

    /**
     * The `Covariant` (and thus `Invariant`) instance for `Id`.
     */
    implicit val IdCovariant: Covariant[Id] =
      new Covariant[Id] {
        def map[A, B](f: A => B): Id[A] => Id[B] = { id =>
          Id(f(Id.unwrap(id)))
        }
      }

    /**
     * The `IdentityBoth` (and `AssociativeBoth`) instance for `Id`.
     */
    implicit val IdIdentityBoth: IdentityBoth[Id] =
      new IdentityBoth[Id] {
        val any: Id[Any] = Id(())

        def both[A, B](fa: => Id[A], fb: => Id[B]): Id[(A, B)] =
          Id(Id.unwrap(fa) -> Id.unwrap(fb))
      }

    /**
     * The `AssociativeFlatten` and `IdentityFlatten` instance for `Id`.
     */
    implicit val IdIdentityFlatten: IdentityFlatten[Id] =
      new IdentityFlatten[Id] {
        def any: Id[Any] = Id(())

        def flatten[A](ffa: Id[Id[A]]): Id[A] = Id.unwrap(ffa)
      }
  }

}
