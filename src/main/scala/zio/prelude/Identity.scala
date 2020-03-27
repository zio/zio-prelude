package zio.prelude

import zio.test.laws.Lawful

trait Identity[A] extends LeftIdentity[A] with RightIdentity[A] {
  def identity: A

  override final def leftIdentity: A = identity

  override final def rightIdentity: A = identity
}

object Identity extends Lawful[Identity with Equal] {

  final val laws = LeftIdentity.laws + RightIdentity.laws

  def apply[A](implicit Identity: Identity[A]): Identity[A] = Identity

  def fromFunctions[A](identity0: A, op: (A, A) => A): Identity[A] =
    new Identity[A] {
      def identity: A = identity0

      def combine(l: A, r: A): A = op(l, r)
    }

  implicit val charIdentity: Identity[Char] =
    Identity.fromFunctions[Char]('\u0000', (l: Char, r: Char) => (l + r).toChar)

  implicit val stringIdentity: Identity[String] =
    Identity.fromFunctions[String]("", (l: String, r: String) => l + r)

  implicit val intIdentity: Identity[Int] =
    Identity.fromFunctions[Int](0, (l: Int, r: Int) => l + r)

  implicit val multIntIdentity: Identity[MultInt] =
    Identity.fromFunctions[MultInt](MultInt(1), (l: MultInt, r: MultInt) => MultInt(l * r))

  implicit val longIdentity: Identity[Long] =
    Identity.fromFunctions[Long](0L, (l: Long, r: Long) => l + r)

  implicit val multLongIdentity: Identity[MultLong] =
    Identity.fromFunctions[MultLong](MultLong(1L), (l: MultLong, r: MultLong) => MultLong(l * r))

  implicit val floatIdentity: Identity[Float] =
    Identity.fromFunctions[Float](0, (l: Float, r: Float) => l + r)

  implicit val doubleIdentity: Identity[Double] =
    Identity.fromFunctions[Double](0, (l: Double, r: Double) => l + r)

  implicit val booleanIdentity: Identity[Boolean] =
    Identity.fromFunctions[Boolean](false, (l: Boolean, r: Boolean) => l || r)

  implicit val conjIdentity: Identity[Conj] =
    Identity.fromFunctions[Conj](Conj(true), (l: Conj, r: Conj) => Conj(l && r))

  implicit def optionIdentity[A: Associative]: Identity[Option[A]] =
    new Identity[Option[A]] {
      def identity: Option[A] = None

      def combine(l: Option[A], r: Option[A]): Option[A] =
        (l, r) match {
          case (Some(l), Some(r)) => Some(l <> r)
          case (Some(l), None)    => Some(l)
          case (None, Some(r))    => Some(r)
          case _                  => None
        }
    }

  implicit def eitherIdentity[E, A: Identity]: Identity[Either[E, A]] =
    new Identity[Either[E, A]] {
      def identity: Either[E, A] = Right(Identity[A].identity)

      def combine(l: Either[E, A], r: Either[E, A]): Either[E, A] =
        (l, r) match {
          case (Left(l), _)         => Left(l)
          case (_, Left(r))         => Left(r)
          case (Right(l), Right(r)) => Right(l <> r)
        }
    }

  implicit def listIdentity[A]: Identity[List[A]] =
    Identity.fromFunctions[List[A]](Nil, _ ++ _)

  implicit def vectorIdentity[A]: Identity[Vector[A]] =
    Identity.fromFunctions[Vector[A]](Vector.empty, _ ++ _)

  implicit def mapIdentity[K, V: Associative]: Identity[Map[K, V]] =
    new Identity[Map[K, V]] {
      def identity: Map[K, V] = Map()

      def combine(l: Map[K, V], r: Map[K, V]): Map[K, V] =
        r.foldLeft(l) {
          case (map, (k, v)) => map.updated(k, map.get(k).fold(v)(_ <> v))
        }
    }

  implicit def setIdentity[A]: Identity[Set[A]] =
    Identity.fromFunctions[Set[A]](Set.empty, _ | _)
}
