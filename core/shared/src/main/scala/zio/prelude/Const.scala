package zio.prelude

/**
 * Defines the `Const` type and its companion. The `zio.prelude` package object
 * mixes this trait in, so `import zio.prelude._` brings `Const` into scope.
 */
trait ConstExports {

  /**
   * The companion of `Const`: `Const(a)` wraps a value and `Const.unwrap`
   * returns it.
   */
  object Const extends NewtypeF

  /**
   * `Const[A, B]` holds a value of type `A` and ignores `B`, a phantom type
   * parameter that appears only in the type. A traversal that uses
   * `Const[A, *]` as its effect collects one `A` per element, which is how
   * `ForEach#foldMap` is written.
   *
   * {{{
   * val label: Const[String, Int] = Const("name")
   *
   * Const.unwrap(label) // "name"
   * }}}
   */
  type Const[+A, +B] = Const.Type[A]
}
