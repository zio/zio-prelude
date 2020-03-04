package zio.prelude

object Tests {
  def test[TypeClass[_], T](implicit ev: TypeClass[T]): Unit = ???

  test[Equal, String]
  test[Equal, Double]
}
