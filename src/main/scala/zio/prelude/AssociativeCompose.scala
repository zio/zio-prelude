package zio.prelude

trait AssociativeCompose[:=>[-_, +_]] {
  def compose[A, B, C](bc: B :=> C, ab: A :=> B): A :=> C

  def associativeCompose[A, B, C, D](
    ab: A :=> B,
    bc: B :=> C,
    cd: C :=> D
  )(implicit eq: Equal[A :=> D]): Boolean = {
    val ad1 = compose(cd, compose(bc, ab))
    val ad2 = compose(compose(cd, bc), ab)

    eq.equal(ad1, ad2)
  }
}

object AssociativeCompose {

  implicit val FunctionBothEitherIdentityCompose
    : ApplicationCompose[Function, ({ type lambda[+f, +s] = (f, s) })#lambda, Function]
      with BothCompose[Function, ({ type lambda[+f, +s] = (f, s) })#lambda]
      with EitherCompose[Function, ({ type lambda[+l, +r] = Either[l, r] })#lambda]
      with IdentityCompose[Function] =
    new ApplicationCompose[Function, ({ type lambda[+f, +s] = (f, s) })#lambda, Function]
      with BothCompose[Function, ({ type lambda[+f, +s] = (f, s) })#lambda]
      with EitherCompose[Function, ({ type lambda[+l, +r] = Either[l, r] })#lambda]
      with IdentityCompose[Function] {

      def identity[A]: A => A = scala.Predef.identity

      def compose[A, B, C](bc: B => C, ab: A => B): A => C =
        bc.compose(ab)

      override def fromFirst[A]: Function[(A, Any), A] = _._1

      override def fromSecond[B]: Function[(Any, B), B] = _._2

      override def toBoth[A, B, C](a2b: Function[A, B])(a2c: Function[A, C]): Function[A, (B, C)] = { a =>
        (a2b(a), a2c(a))
      }

      override def application[A, B]: Function[(Function[A, B], A), B] = { case (a2b, a) =>
        a2b(a)
      }

      override def curry[A, B, C](f: Function[(A, B), C]): Function[A, Function[B, C]] = { a => b =>
        f((a, b))
      }

      override def uncurry[A, B, C](g: Function[A, Function[B, C]]): Function[(A, B), C] = { case (a, b) =>
        g(a)(b)
      }

      override def toLeft[A]: Function[A, Either[A, Nothing]] = Left(_)

      override def toRight[B]: Function[B, Either[Nothing, B]] = Right(_)

      override def fromEither[A, B, C](a2c: => Function[A, C])(b2c: => Function[B, C]): Function[Either[A, B], C] = {
        case Left(a)  => a2c(a)
        case Right(b) => b2c(b)
      }
    }
}

trait AssociativeComposeSyntax {
  implicit class AssociativeComposeOps[A, B, :=>[-_, +_]](private val ab: A :=> B) {

    /** A symbolic alias for `andThen`. Composes `A -> B` with `B -> C` to form `A -> C`. */
    def >>>[C](implicit ev: AssociativeCompose[:=>]): (B :=> C) => (A :=> C) = { bc =>
      ev.compose(bc, ab)
    }

    /** Composes `A -> B` with `B -> C` to form `A -> C`. */
    def andThen[C](implicit ev: AssociativeCompose[:=>]): (B :=> C) => (A :=> C) = { bc =>
      ev.compose(bc, ab)
    }

    /** A symbolic alias for `compose`. Composes `B <- A` with `A <- Z` to form `B <- Z`. */
    def <<<[Z](implicit ev: AssociativeCompose[:=>]): (Z :=> A) => Z :=> B = { za =>
      ev.compose(ab, za)
    }

    /** Composes `B <- A` with `A <- Z` to form `B <- Z`. */
    def compose[Z](implicit ev: AssociativeCompose[:=>]): (Z :=> A) => Z :=> B = { za =>
      ev.compose(ab, za)
    }
  }
}
