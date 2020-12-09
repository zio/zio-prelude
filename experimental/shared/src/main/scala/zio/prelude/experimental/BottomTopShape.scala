package zio.prelude
package experimental

trait BottomTopShape[A] extends JoinMeetShape[A] {

  override type Join[x] <: Identity[x]

  override type Meet[x] <: Identity[x]

  def bottom: A = Join.identity

  def top: A = Meet.identity

}

object BottomTopShape {

  type Aux[A, +join[x] <: Identity[x], +meet[x] <: Identity[x]] = BottomTopShape[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

}
