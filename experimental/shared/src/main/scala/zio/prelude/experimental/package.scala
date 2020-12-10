package zio.prelude

package object experimental extends ComplementShapeSyntax with JoinMeetSyntax {

  object classic {

    import zio.prelude.classic._

    type Lattice[A]                  = JoinMeetShape.Aux[A, Semilattice, Semilattice]
    type BoundedLattice[A]           = JoinMeetShape.Aux[A, BoundedSemilattice, BoundedSemilattice]
    type OrthoComplementedLattice[A] = Complement[A] with Involution[A] {
      type Join[x] = BoundedSemilattice[x]
      type Meet[x] = BoundedSemilattice[x]
    }
    type DistributiveLattice[A]      = DistributiveJoinMeet.Aux[A, Semilattice, Semilattice]
    type BooleanAlgebra[A]           = Absorption[A] with Complement[A] with DistributiveJoinMeet[A] {
      type Join[x] = BoundedSemilattice[x]
      type Meet[x] = BoundedSemilattice[x]
    }
  }

}
