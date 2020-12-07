package zio.prelude

package object experimental extends ComplementShapeSyntax with JoinMeetSyntax {

  object classic {

    import zio.prelude.classic._

    type Lattice[A]                  = Absorption[A, Semilattice, Semilattice]
    type BoundedLattice[A]           = Absorption[A, BoundedSemilattice, BoundedSemilattice]
    type OrthoComplementedLattice[A] = BoundedLattice[A]
      with Complement[A, BoundedSemilattice, BoundedSemilattice]
      with Involution[A, BoundedSemilattice, BoundedSemilattice]
    type DistributiveLattice[A]      = Lattice[A] with DistributiveJoinMeet[A, Semilattice, Semilattice]

  }

}
