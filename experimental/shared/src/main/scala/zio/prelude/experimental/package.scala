package zio.prelude

package object experimental extends AddMultiplyShapeSyntax with DivideShapeSyntax with SubtractShapeSyntax {

  object classic {

    import zio.prelude.classic._

    type Semiring[A] = Annihilation[A] with DistributiveMultiply[A] {
      type Addition[x] <: CommutativeMonoid[x]
      type Multiplication[x] <: Identity[x]
    }
    type Ring[A]     = Semiring[A] with SubtractShape[A] {
      type Addition[x] <: AbelianGroup[x]
      type Multiplication[x] <: Identity[x]
    }
    type Field[A]    = Ring[A] with DivideShape[A] {
      type Addition[x] <: AbelianGroup[x]
      type Multiplication[x] <: InverseNonZero[x]
    }

  }

}
