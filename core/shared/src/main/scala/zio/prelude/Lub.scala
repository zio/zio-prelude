package zio.prelude

import zio.prelude.NotSubType.<:!<

trait Lub[-A, -B, +Out] {
  def left(a: A): Out
  def right(b: B): Out
}

trait LowPriorityLUB {

  def instance[A, B, Out](f: A => Out, g: B => Out): Lub[A, B, Out] = new Lub[A, B, Out] {
    override def left(a: A): Out  = f(a)
    override def right(b: B): Out = g(b)
  }

  implicit def bSubtypeA[A, B <: A]: Lub[A, B, A] = instance(identity, identity)
}

object Lub extends LowPriorityLUB {
  implicit def aSubtypeB[A <: B, B]: Lub[A, B, B]                                  = instance(identity, identity)
  implicit def default[A, B](implicit ev: A <:!< B, ev1: B <:!< A): Lub[A, B, Any] =
    instance(identity, identity)
}
