package zio.prelude

package object experimental extends ComplementShapeSyntax with JoinMeetSyntax {

  object classic {

    import zio.prelude.classic._

    type Lattice[A]                  = Absorption.Aux[A, Semilattice, Semilattice]
    type BoundedLattice[A]           = Absorption.Aux[A, BoundedSemilattice, BoundedSemilattice]
    type OrthoComplementedLattice[A] = BoundedLattice[A]
      with Complement.Aux[A, BoundedSemilattice, BoundedSemilattice]
      with Involution.Aux[A, BoundedSemilattice, BoundedSemilattice]
    type DistributiveLattice[A]      = Lattice[A] with DistributiveJoinMeet.Aux[A, Semilattice, Semilattice]

  }

}
