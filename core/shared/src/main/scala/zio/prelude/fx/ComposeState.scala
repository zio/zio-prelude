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

  final case class RightIdentity[S1, S2, S3]() extends ComposeState[S1, S2, Unit, Unit] {
    type In  = S1
    type Out = S2
    def compose(left: S1 => S2, right: Unit => Unit): In => Out =
      left
  }

  final case class Compose[S1, S2 <: S3, S3, S4]() extends ComposeState[S1, S2, S3, S4] {
    type In  = S1
    type Out = S4
    def compose(left: S1 => S2, right: S3 => S4): In => Out =
      right compose left
  }

  final case class RightConst[S1, S2, S3 >: Unit, S4]() extends ComposeState[S1, S2, S3, S4] {
    type In  = S1
    type Out = S4
    def compose(left: S1 => S2, right: S3 => S4): In => Out =
      _ => right(())
  }

  final case class LeftConst[S1 >: Unit, S2, S3, S4]() extends ComposeState[S1, S2, S3, S4] {
    type In  = S3
    type Out = S4
    def compose(left: S1 => S2, right: S3 => S4): In => Out =
      right
  }

  implicit def leftIdentity[S1, S2]: Aux[Unit, Unit, S1, S2, S1, S2] =
    ComposeState.LeftIdentity()
}

trait ComposeStateImplicits extends ComposeStateLowPriorityImplicits {
  implicit def rightIdentity[S1, S2]: ComposeState.Aux[S1, S2, Unit, Unit, S1, S2] =
    ComposeState.RightIdentity()
}

trait ComposeStateLowPriorityImplicits extends ComposeStateLowPriorityImplicits2 {
  implicit def compose[S1, S2 <: S3, S3, S4]: ComposeState.Aux[S1, S2, S3, S4, S1, S4] =
    ComposeState.Compose()
}

trait ComposeStateLowPriorityImplicits2 {
  implicit def rightConst[S1, S2, S3 >: Unit, S4]: ComposeState.Aux[S1, S2, S3, S4, S1, S4] =
    ComposeState.RightConst()

  // implicit def leftConst[S1 >: Unit, S2, S3, S4]: ComposeState.Aux[S1, S2, S3, S4, S3, S4] =
  //   ComposeState.LeftConst()
}
