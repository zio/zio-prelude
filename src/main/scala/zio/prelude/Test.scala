package zio.prelude

object Tests {
  implicit def additionCommutativeAssociative: Commutative[Int] with Associative[Int] =
    new Commutative[Int] with Associative[Int] {
      def combine(l: => Int, r: => Int) = l + r
    }

  def test[TypeClass[_], T](implicit ev: TypeClass[T]): Unit = { val _ = ev }

  trait CollectionLike[F[_]] {
    def empty[A]: F[A]
    def concat[A](l: F[A], r: F[A]): F[A]
  }
  object CollectionList {
    implicit val ListCollectionLike =
      new CollectionLike[List] {
        def empty[A]: List[A]                          = Nil
        def concat[A](l: List[A], r: List[A]): List[A] = l ++ r
      }
  }

  test[Equal, String]
  test[Equal, Double]

  test[Commutative, Int]
  test[Closure, Int]
  test[Associative, Int]

  trait Animal
  trait Dog extends Animal
  val scotty = new Dog {}
  val stray  = new Dog {}

  implicit val equalAnimal: Equal[Animal] = Equal.default
  implicit val equalDog: Equal[Dog]       = Equal.default

  scotty === stray // No ambiguous implicit

  trait Dummy[A]

  object Mult extends Subtype[Int]
  type Mult = Mult.Type

  def acceptInt(i: Int): Unit   = println(i)
  def acceptMult(m: Mult): Unit = println(m)

  acceptInt(Mult(23)) // Mult is a subtype of Int
  // acceptMult(23)

  Mult(42) match {
    case Mult(42) => println("It's 42!") // Irrefutable
  }

  Equal[Mult].equal(Mult(23), Mult(342))

  val v1 = Validation.succeed(42)
  val v2 = Validation.fail(new Exception("Uh oh!"))
  val v3 = Validation.succeed("foo")
  val v4 = Validation.fromTry(scala.util.Try(???))

  val v5 =
    v1 zipPar v2 zipPar v3 zipPar v4
}
