package zio.prelude

trait ConstExports {
  object Const extends NewtypeF
  type Const[+A, +B] = Const.Type[A]
}
