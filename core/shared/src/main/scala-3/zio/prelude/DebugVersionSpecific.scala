package zio.prelude

import Debug.*

trait DebugVersionSpecific:

  given[A](using Debug[A]): Debug[IArray[A]] with
    def debug(array: IArray[A]): Repr =
      Repr.VConstructor(List("scala"), "IArray", array.toList.map(_.debug))
