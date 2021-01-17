package zio.prelude

package object experimental
    extends AddMultiplyShapeSyntax
    with ApplicationComposeSyntax
    with BothComposeSyntax
    with DivideShapeSyntax
    with EitherComposeSyntax
    with PartialDivideShapeSyntax
    with SubtractShapeSyntax {

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
    type Field[A]    = Ring[A] with PartialDivideShape[A] {
      type Addition[x] <: AbelianGroup[x]
      type Multiplication[x] <: PartialInverse[x]
    }

    type CartesianCategory[=>:[-_, +_], :*:[+_, +_]]                     = Category[=>:] with BothCompose.Aux[=>:, :*:]
    type ClosedCartesianCategory[=>:[-_, +_], :*:[+_, +_], -->:[-_, +_]] = CartesianCategory[=>:, :*:]
      with ApplicationCompose.Aux[=>:, :*:, -->:]
    type CoCartesianCategory[=>:[-_, +_], :+:[+_, +_]]                   = Category[=>:] with EitherCompose.Aux[=>:, :+:]
  }

}
