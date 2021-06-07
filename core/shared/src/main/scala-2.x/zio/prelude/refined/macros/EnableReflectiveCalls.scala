package zio.prelude.refined.macros

import scala.reflect.macros.whitebox

object EnableReflectiveCalls {

  def apply(c: whitebox.Context) = {
    import c.universe._
    q"import _root_.scala.language.reflectiveCalls" ::
      q"Nil.asInstanceOf[{ def size }].size" ::
      Nil
  }
}
