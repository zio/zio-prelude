package zio.prelude
package experimental

trait TopShape[A] extends JoinMeetShape[A] {

  override type Meet[x] <: Identity[x]

  def top: A = Meet.identity

}

object TopShape {

  type Aux[A, +join[x] <: Associative[x], +meet[x] <: Identity[x]] = TopShape[A] {
    type Join[x] <: join[x]
    type Meet[x] <: meet[x]
  }

}
