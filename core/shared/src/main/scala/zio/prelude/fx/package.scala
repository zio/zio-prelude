package zio.prelude

package object fx {
  type Cause[+E] = Semiring[Nothing, E]
  val Cause: Semiring.type = Semiring
}
