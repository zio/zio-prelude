package zio.prelude.fx

sealed trait ComposeState[S1, S2, S3, S4] {
  type In
  type Out
  def compose(left: S1 => S2, right: S3 => S4): In => Out
}

object ComposeState extends ComposeStateImplicits {
  type Aux[S1, S2, S3, S4, In0, Out0] = ComposeState[S1, S2, S3, S4] { type In = In0; type Out = Out0 }

  def apply[S1, S2, S3, S4](implicit ev: ComposeState[S1, S2, S3, S4]): Aux[S1, S2, S3, S4, ev.In, ev.Out] = ev

  final case class LeftIdentity[S1, S2]() extends ComposeState[Unit, Unit, S1, S2] {
    type In  = S1
    type Out = S2
    def compose(left: Unit => Unit, right: S1 => S2): In => Out =
      right
  }

  final case class RightIdentity[S1, S2]() extends ComposeState[S1, S2, Unit, Unit] {
    type In  = S1
    type Out = S2
    def compose(left: S1 => S2, right: Unit => Unit): In => Out =
      left
  }

  final case class Compose[S1, S2, S3]() extends ComposeState[S1, S2, S2, S3] {
    type In  = S1
    type Out = S3
    def compose(left: S1 => S2, right: S2 => S3): In => Out =
      right compose left
  }

  implicit def leftIdentity[S1, S2]: Aux[Unit, Unit, S1, S2, S1, S2] =
    ComposeState.LeftIdentity()
}

trait ComposeStateImplicits extends ComposeStateLowPriorityImplicits {
  implicit def rightIdentity[S1, S2]: ComposeState.Aux[S1, S2, Unit, Unit, S1, S2] =
    ComposeState.RightIdentity()
}

trait ComposeStateLowPriorityImplicits {
  implicit def compose[S1, S2, S3]: ComposeState.Aux[S1, S2, S2, S3, S1, S3] =
    ComposeState.Compose()
}

object Example {

  def compose[S1, S2, S3, S4, In, Out](left: S1 => S2, right: S3 => S4)(implicit
    ev: ComposeState.Aux[S1, S2, S3, S4, In, Out]
  ): In => Out =
    ev.compose(left, right)

  lazy val unitUnitState: Unit => Unit   = ???
  lazy val stringIntState: String => Int = ???
  lazy val intDoubleState: Int => Double = ???

  val x = compose(unitUnitState, stringIntState)
  val y = compose(intDoubleState, unitUnitState)
  val z = compose(stringIntState, intDoubleState)

  val test1: String => Int    = x
  val test2: Int => Double    = y
  val test3: String => Double = z
}
