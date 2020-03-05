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
}
