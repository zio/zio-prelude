package zio.prelude

package object fx {
  type Cause[+E] = ParSeq[Nothing, E]
  val Cause: ParSeq.type = ParSeq
}
