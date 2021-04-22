package zio.prelude.fx

sealed trait FoldState[S1, S2, S3, S4, S5, S6] {
  type In
  type Out
  def success(left: S1 => S2, right: S3 => S4): In => Out
  def failure(left: S1 => S2, right: S5 => S6): In => Out
}

object FoldState extends FoldStateImplicits {
  type Aux[S1, S2, S3, S4, S5, S6, In0, Out0] = FoldState[S1, S2, S3, S4, S5, S6] { type In = In0; type Out = Out0 }

  def apply[S1, S2, S3, S4, S5, S6](implicit
    ev: FoldState[S1, S2, S3, S4, S5, S6]
  ): Aux[S1, S2, S3, S4, S5, S6, ev.In, ev.Out] = ev

  final case class LeftIdentity[S1, S2]() extends FoldState[Unit, Unit, S1, S2, S1, S2] {
    type In  = S1
    type Out = S2
    def success(left: Unit => Unit, right: S1 => S2): In => Out =
      right
    def failure(left: Unit => Unit, right: S1 => S2): In => Out =
      right
  }

  final case class RightIdentity[S1, S2]() extends FoldState[S1, S2, Unit, Unit, S1, S2] {
    type In  = S1
    type Out = S2
    def failure(left: S1 => S2, right: S1 => S2): In => Out     =
      right
    def success(left: S1 => S2, right: Unit => Unit): In => Out =
      left
  }

  final case class RightIdentity2[S1, S2]() extends FoldState[S1, S2, Unit, Unit, Unit, Unit] {
    type In  = S1
    type Out = S2
    def failure(left: S1 => S2, right: Unit => Unit): In => Out =
      left
    def success(left: S1 => S2, right: Unit => Unit): In => Out =
      left
  }

  final case class Compose[S1, S2, S3]() extends FoldState[S1, S2, S2, S3, S1, S3] {
    type In  = S1
    type Out = S3
    def failure(left: S1 => S2, right: S1 => S3): In => Out =
      right
    def success(left: S1 => S2, right: S2 => S3): In => Out =
      right compose left
  }

  implicit def leftIdentity[S1, S2]: Aux[Unit, Unit, S1, S2, S1, S2, S1, S2] =
    FoldState.LeftIdentity()
}

trait FoldStateImplicits extends FoldStateLowPriorityImplicits {
  implicit def rightIdentity[S1, S2]: FoldState.Aux[S1, S2, Unit, Unit, S1, S2, S1, S2] =
    FoldState.RightIdentity()
}

trait FoldStateLowPriorityImplicits extends FoldStateLowPriorityImplicits2 {
  implicit def compose[S1, S2, S3]: FoldState.Aux[S1, S2, S2, S3, S1, S3, S1, S3] =
    FoldState.Compose()
}

trait FoldStateLowPriorityImplicits2 {
  implicit def rightIdentity2[S1, S2]: FoldState.Aux[S1, S2, Unit, Unit, Unit, Unit, S1, S2] =
    FoldState.RightIdentity2()
}
