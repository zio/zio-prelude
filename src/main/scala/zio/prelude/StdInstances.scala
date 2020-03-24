package zio.prelude

import scala.annotation.tailrec

trait StdInstances {

  type OrdWithHash[A] = Ord[A] with Hash[A]

  /**
   * Constructs an `Ord[A]` from a [[`scala.math.Ordering]].
   */
  def default[A](implicit ord: scala.math.Ordering[A]): OrdWithHash[A] = OrdWithHash[A] {
    (l, r) => Ordering.fromCompare(ord.compare(l, r))
  }

  /**
   * Ordering and Hash for `Unit` values.
   */
  implicit val unit: OrdWithHash[Unit] = default[Unit]

  /**
   * Ordering and Hash for `Boolean` values.
   */
  implicit val boolean: OrdWithHash[Boolean] = default[Boolean]

  /**
   * Ordering and Hash for `Byte` values.
   */
  implicit val byte: OrdWithHash[Byte] = default[Byte]

  /**
   * Ordering and Hash for `Char` values.
   */
  implicit val char: OrdWithHash[Char] = default[Char]

  /**
   * Ordering and Hash for `String` values.
   */
  implicit val string: OrdWithHash[String] = default[String]

  /**
   * Ordering and Hash for `Short` values.
   */
  implicit val short: OrdWithHash[Short] = default[Short]

  /**
   * Ordering and Hash for `Int` values.
   */
  implicit val int: OrdWithHash[Int] = default[Int]

  /**
   * Ordering and Hash for `Long` values.
   */
  implicit val long: OrdWithHash[Long] = default[Long]

  /**
   * Ordering and Hash for `Float` values. Note that to honor the contract that a
   * value is always equal to itself, comparing `Float.NaN` with itself will
   * return `true`, which is different from the behavior of `Float#equals`.
   */
  implicit val float: OrdWithHash[Float] = OrdWithHash[Float] {
    (l, r) =>
      if (l.isNaN && r.isNaN) Ordering.Equals
      else Ordering.fromCompare(java.lang.Float.compare(l, r))
  }

  /**
   * Ordering and Hash for `Double` values. Note that to honor the contract that a
   * value is always equal to itself, comparing `Double.NaN` with itself will
   * return `true`, which is different from the behavior of `Double#equals`.
   */
  implicit val double: OrdWithHash[Double] = OrdWithHash[Double] {
    (l, r) =>
      if (l.isNaN && r.isNaN) Ordering.Equals
      else Ordering.fromCompare(java.lang.Double.compare(l, r))
  }

  /**
   * Ordering and Hash for `Nothing` values. Note that since there are not values of
   * type `Nothing` the `equals` method of this instance can never be called
   * but it can be useful in deriving instances for more complex types.
   */
  implicit val nothing: OrdWithHash[Nothing] = OrdWithHash[Nothing]((l, _) => l)


  /**
   * Derives an `Ord[Option[A]]` given an `Ord[A]`. `None` will be treated as
   * less than all other values.
   */
  implicit def option[A: OrdWithHash]: OrdWithHash[Option[A]] =
    OrdWithHash[Option[A]] { (l, r)  =>
      (l, r) match {
        case (None, None)       => Ordering.Equals
        case (Some(l), Some(r)) => l =?= r
        case (Some(_), None)    => Ordering.GreaterThan
        case _                  => Ordering.LessThan
      }
  }


  /**
   * Derives an `Equal[Either[A, B]]` given an `Equal[A]` and an `Equal[B]`.
   */
  implicit def either[A: OrdWithHash, B: OrdWithHash]: OrdWithHash[Either[A, B]] = new Ord[Either[A, B]] with Hash[Either[A, B]] {
    override def compare(l: Either[A, B], r: Either[A, B]): Ordering = (Ord[A] either Ord[B]).compare(l, r)

    override def hash(a: Either[A, B]): Int = (Hash[A] either Hash[B]).hashCode()
  }

  /**
   * Derives an `Equal[List[A]]` given an `Equal[A]`.
   */
  implicit def list[A: OrdWithHash]: OrdWithHash[List[A]] = new Ord[List[A]] with Hash[List[A]] {
    override def compare(l: List[A], r: List[A]): Ordering = {

      @tailrec
      def loop[A: Ord](left: List[A], right: List[A]): Ordering =
        (left, right) match {
          case (Nil, Nil) => Ordering.Equals
          case (Nil, _) => Ordering.LessThan
          case (_, Nil) => Ordering.GreaterThan
          case ((h1 :: t1), (h2 :: t2)) =>
            val compare = Ord[A].compare(h1, h2)
            if (compare.isEqual) loop(t1, t2) else compare
        }

      loop(l, r)
    }

    override def hash(a: List[A]): Int = a.hashCode()
  }

  implicit def vector[A: OrdWithHash]: OrdWithHash[Vector[A]] = new Ord[Vector[A]] with Hash[Vector[A]] {
    override def compare(l: Vector[A], r: Vector[A]): Ordering = {
      val j = l.length
      val k = r.length

      def loop(i: Int): Ordering =
        if (i == j && i == k) Ordering.Equals
        else if (i == j) Ordering.LessThan
        else if (i == k) Ordering.GreaterThan
        else {
          val compare = Ord[A].compare(l(i), r(i))
          if (compare.isEqual) loop(i + 1) else compare
        }

      loop(0)
    }

    override def hash(a: Vector[A]): Int = a.hashCode()
  }

  /**
   * Equality for `Set[A]` values. Due to the limitations of Scala's `Set`,
   * this uses object equality and hash code on the elements.
   */
  implicit def set[A: Hash]: Hash[Set[A]] = new Hash[Set[A]] {
    override def equal(set1: Set[A], set2: Set[A]): Boolean = set1 == set2

    override def hash(set: Set[A]): Int = set.hashCode()
  }

  /**
   * Derives an `Equal[Map[A, B]]` given an `Equal[B]`. Due to the limitations
   * of Scala's `Map`, this uses object equality and hash code on the keys.
   */
  implicit def map[K: Hash, V: Hash]: Hash[Map[K, V]] = new Hash[Map[K, V]] {
    override def equal(map1: Map[K, V], map2: Map[K, V]): Boolean = {
      map1.size == map2.size &&
        map1.forall { case (key, value) => map2.get(key).fold(false)(_ === value) }
    }

    override def hash(map: Map[K, V]): Int = map.hashCode()
  }


  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple2Ord[A: OrdWithHash, B: OrdWithHash]: OrdWithHash[(A, B)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case ((a1, b1), (a2, b2)) => (a1 =?= a2) <> (b1 =?= b2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple3Ord[A: OrdWithHash, B: OrdWithHash, C: OrdWithHash]: OrdWithHash[(A, B, C)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1), (a2, b2, c2)) => (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple4Ord[A: OrdWithHash, B: OrdWithHash, C: OrdWithHash, D: OrdWithHash]: OrdWithHash[(A, B, C, D)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple5Ord[A: OrdWithHash, B: OrdWithHash, C: OrdWithHash, D: OrdWithHash, E: OrdWithHash]: OrdWithHash[(A, B, C, D, E)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple6Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple7Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple8Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash
  ]
  : OrdWithHash[(A, B, C, D, E, F, G, H)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1, g1, h1), (a2, b2, c2, d2, e2, f2, g2, h2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple9Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1, g1, h1, i1), (a2, b2, c2, d2, e2, f2, g2, h2, i2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple10Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple11Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash,
    K: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J, K)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple12Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash,
    K: OrdWithHash,
    L: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case ((a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1), (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2)) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple13Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash,
    K: OrdWithHash,
    L: OrdWithHash,
    M: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2)
          ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple14Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash,
    K: OrdWithHash,
    L: OrdWithHash,
    M: OrdWithHash,
    N: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2)
          ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple15Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash,
    K: OrdWithHash,
    L: OrdWithHash,
    M: OrdWithHash,
    N: OrdWithHash,
    O: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2)
          ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple16Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash,
    K: OrdWithHash,
    L: OrdWithHash,
    M: OrdWithHash,
    N: OrdWithHash,
    O: OrdWithHash,
    P: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2)
          ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple17Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash,
    K: OrdWithHash,
    L: OrdWithHash,
    M: OrdWithHash,
    N: OrdWithHash,
    O: OrdWithHash,
    P: OrdWithHash,
    Q: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2)
          ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple18Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash,
    K: OrdWithHash,
    L: OrdWithHash,
    M: OrdWithHash,
    N: OrdWithHash,
    O: OrdWithHash,
    P: OrdWithHash,
    Q: OrdWithHash,
    R: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2)
          ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple19Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash,
    K: OrdWithHash,
    L: OrdWithHash,
    M: OrdWithHash,
    N: OrdWithHash,
    O: OrdWithHash,
    P: OrdWithHash,
    Q: OrdWithHash,
    R: OrdWithHash,
    S: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2)
          ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple20Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash,
    K: OrdWithHash,
    L: OrdWithHash,
    M: OrdWithHash,
    N: OrdWithHash,
    O: OrdWithHash,
    P: OrdWithHash,
    Q: OrdWithHash,
    R: OrdWithHash,
    S: OrdWithHash,
    T: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2)
          ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2) <> (t1 =?= t2)
      }
    }


  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple21Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash,
    K: OrdWithHash,
    L: OrdWithHash,
    M: OrdWithHash,
    N: OrdWithHash,
    O: OrdWithHash,
    P: OrdWithHash,
    Q: OrdWithHash,
    R: OrdWithHash,
    S: OrdWithHash,
    T: OrdWithHash,
    U: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2)
          ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2) <> (t1 =?= t2) <> (u1 =?= u2)
      }
    }

  /**
   * Derives an `Ord` for a product type given an `Ord` for each element of
   * the product type.
   */
  implicit def Tuple22Ord[
    A: OrdWithHash,
    B: OrdWithHash,
    C: OrdWithHash,
    D: OrdWithHash,
    E: OrdWithHash,
    F: OrdWithHash,
    G: OrdWithHash,
    H: OrdWithHash,
    I: OrdWithHash,
    J: OrdWithHash,
    K: OrdWithHash,
    L: OrdWithHash,
    M: OrdWithHash,
    N: OrdWithHash,
    O: OrdWithHash,
    P: OrdWithHash,
    Q: OrdWithHash,
    R: OrdWithHash,
    S: OrdWithHash,
    T: OrdWithHash,
    U: OrdWithHash,
    V: OrdWithHash
  ]: OrdWithHash[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    OrdWithHash { (a, b) =>
      (a, b) match {
        case (
          (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1, q1, r1, s1, t1, u1, v1),
          (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2, q2, r2, s2, t2, u2, v2)
          ) =>
          (a1 =?= a2) <> (b1 =?= b2) <> (c1 =?= c2) <> (d1 =?= d2) <> (e1 =?= e2) <> (f1 =?= f2) <> (g1 =?= g2) <> (h1 =?= h2) <> (i1 =?= i2) <> (j1 =?= j2) <> (k1 =?= k2) <> (l1 =?= l2) <> (m1 =?= m2) <> (n1 =?= n2) <> (o1 =?= o2) <> (p1 =?= p2) <> (q1 =?= q2) <> (r1 =?= r2) <> (s1 =?= s2) <> (t1 =?= t2) <> (u1 =?= u2) <> (v1 =?= v2)
      }
    }
}

object OrdWithHash {

  def apply[A](c0: (A, A) => Ordering): OrdWithHash[A] = new Ord[A] with Hash[A] {
    override def compare(l: A, r: A): Ordering = c0(l, r)

    override def hash(a: A): Int = a.hashCode()
  }
}
