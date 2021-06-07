package zio.prelude.refined

import zio.prelude.refined.macros.Macros

trait SubtypeSmart[A] extends NewtypeSmart[A] {
  type Type <: A

  class Cool()
}

object SubtypeSmart {
  def apply[A](assertion: Assertion[A]): SubtypeSmart[A] =
    macro Macros.make_impl[A, SubtypeSmart[A]]
}
