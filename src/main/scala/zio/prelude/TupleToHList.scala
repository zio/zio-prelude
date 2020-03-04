package zio.prelude

import zio.prelude.HList._

trait TupleToHList[A, B] extends (A => B) {
  def apply(tuple: A): B
}

object TupleToHList {

  implicit def Tuple2ToHList[A, B]: TupleToHList[(A, B), A :*: B :*: Empty] = {
    case (a, b) => a :*: b :*: Empty
  }

  implicit def Tuple3ToHList[A, B, C]: TupleToHList[(A, B, C), A :*: B :*: C :*: Empty] = {
    case (a, b, c) => a :*: b :*: c :*: Empty
  }
}
