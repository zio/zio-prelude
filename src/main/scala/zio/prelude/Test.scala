package zio.prelude

object Tests {
  implicit def additionCommutative: Commutative[Int] = ???
  implicit def additionAssociative: Associative[Int] = ???

  def test[TypeClass[_], T](implicit ev: TypeClass[T]): Unit = ???

  test[Equal, String]
  test[Equal, Double]

  test[Closure, Int]
  test[Commutative, Int]
  test[Associative, Int]


  val Mult = newtype[Int]
  type Mult = Mult.Type 

  def acceptInt(i: Int): Unit = println(i)
  def acceptMult(m: Mult): Unit = println(m)

  acceptInt(Mult(23))
  // acceptMult(23)
}
