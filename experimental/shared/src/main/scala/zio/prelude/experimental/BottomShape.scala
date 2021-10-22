package zio.prelude
package experimental

trait BottomShape[A] extends JoinMeetShape[A] {

  override type Join[x] <: Identity[x]

  def bottom: A = Join.identity

}

object BottomShape {

  type Aux[A, +join[x] <: Identity[x], +meet[x] <: Associative[x]] = BottomShape[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

}
