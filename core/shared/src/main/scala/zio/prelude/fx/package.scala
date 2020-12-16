package zio.prelude

package object fx {
  type Cause[+E] = Semiring[E]
  val Cause: Semiring.type = Semiring
}
