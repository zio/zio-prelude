package zio.prelude

trait BottomTopShape[A, +Join[x] <: Identity[x], +Meet[x] <: Identity[x]] extends JoinMeet[A, Join, Meet] {
  def bottom: A = Join.identity
  def top: A    = Meet.identity
}
