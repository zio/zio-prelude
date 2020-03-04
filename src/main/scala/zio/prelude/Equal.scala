package zio.prelude

sealed trait Equal[-A] { self =>

  def equal(l: A, r: A): Boolean

  final def both[B](that: Equal[B]): Equal[(A, B)] =
    bothWith(that)(identity)

  final def bothWith[B, C](that: Equal[B])(f: C => (A, B)): Equal[C] =
    Equal { (c1, c2) =>
      (f(c1), f(c2)) match {
        case ((a1, b1), (a2, b2)) => self.equal(a1, a2) && that.equal(b1, b2)
      }
    }

  final def contramap[B](f: B => A): Equal[B] =
    Equal((b1, b2) => equal(f(b1), f(b2)))

  final def either[B](that: Equal[B]): Equal[Either[A, B]] =
    eitherWith(that)(identity)

  final def eitherWith[B, C](that: Equal[B])(f: C => Either[A, B]): Equal[C] =
    Equal { (c1, c2) =>
      (f(c1), f(c2)) match {
        case (Left(a1), Left(a2))   => self.equal(a1, a2)
        case (Right(b1), Right(b2)) => that.equal(b1, b2)
        case _                      => false
      }
    }

  final def notEqual(l: A, r: A): Boolean = !equal(l, r)

  final def reflexiveLaw(a: A): Boolean = equal(a, a)

  final def symmetryLaw(a1: A, a2: A): Boolean = equal(a2, a1) ==> equal(a1, a2)

  final def transitivityLaw(a1: A, a2: A, a3: A): Boolean =
    equal(a1, a2) && equal(a2, a3) ==> equal(a1, a3)
}

object Equal {

  def apply[A](implicit equal: Equal[A]): Equal[A] = equal

  def apply[A](eq0: (A, A) => Boolean): Equal[A] =
    new Equal[A] {
      def equal(l: A, r: A): Boolean = refEq(l, r) || eq0(l, r)
    }

  def default[A]: Equal[A] = Equal((l, r) => l == r)

  implicit val ByteEqual: Equal[Byte]       = default[Byte]
  implicit val CharEqual: Equal[Char]       = default[Char]
  implicit val DoubleEqual: Equal[Double]   = Equal(_ == _) // FIXME
  implicit val FloatEqual: Equal[Float]     = default[Float]
  implicit val IntEqual: Equal[Int]         = default[Int]
  implicit val LongEqual: Equal[Long]       = default[Long]
  implicit val NothingEqual: Equal[Nothing] = Equal[Nothing]((l: Nothing, _: Nothing) => l)
  implicit val StringEqual: Equal[String]   = default[String]
  implicit val UnitEqual: Equal[Unit]       = Equal((_, _) => true)

  implicit def EitherEqual[A: Equal, B: Equal]: Equal[Either[A, B]] =
    Equal[A] either Equal[B]

  implicit def ListEqual[A: Equal]: Equal[List[A]] =
    Equal(_.corresponds(_)(Equal[A].equal))

  implicit def MapEqual[A: Equal, B: Equal]: Equal[Map[A, B]] =
    Equal { (map1, map2) =>
      map1.size == map2.size &&
      map1.forall {
        case (a1, b1) =>
          map2.exists {
            case (a2, b2) =>
              Equal[A].equal(a1, a2) && Equal[B].equal(b1, b2)
          }
      }
    }

  implicit def OptionEqual[A: Equal]: Equal[Option[A]] =
    Equal { (o1, o2) =>
      (o1, o2) match {
        case (None, None)         => true
        case (Some(a1), Some(a2)) => Equal[A].equal(a1, a2)
        case _                    => false
      }
    }

  implicit def SetEqual[A: Equal]: Equal[Set[A]] =
    Equal { (a1, a2) =>
      a1.size == a2.size && a1.forall(a => a2.exists(Equal[A].equal(a, _)))
    }

  implicit def TupleEqual[A, B](implicit tupleToHList: TupleToHList[A, B], equal: Equal[B]): Equal[A] =
    equal.contramap(tupleToHList)

  implicit def VectorEqual[A: Equal]: Equal[Vector[A]] =
    Equal(_.corresponds(_)(Equal[A].equal))

  implicit val HListEmptyEqual: Equal[HList.Empty] = Equal((_, _) => true)

  implicit def HListConsEqual[A: Equal, B <: HList](implicit equal: Equal[B]): Equal[HList.Cons[A, B]] =
    Equal[A].bothWith(equal) { case HList.Cons(head, tail) => (head, tail) }

  private def refEq[A](l: A, r: A): Boolean =
    l.asInstanceOf[AnyRef] eq r.asInstanceOf[AnyRef]
}

trait EqualSyntax {

  implicit class EqualSyntax[A](l: A) {
    def ===(r: A)(implicit equal: Equal[A]): Boolean = equal.equal(l, r)

    def !==(r: A)(implicit equal: Equal[A]): Boolean = equal.notEqual(l, r)
  }
}
