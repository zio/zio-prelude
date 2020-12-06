package zio.prelude

package object experimental extends ApplicationComposeSyntax with BothComposeSyntax with EitherComposeSyntax {

  object classic {

    import zio.prelude.classic._

    type CartesianCategory[:=>[-_, +_], :*:[+_, +_]]                     = Category[:=>] with BothCompose[:=>, :*:]
    type ClosedCartesianCategory[:=>[-_, +_], :*:[+_, +_], :-->[-_, +_]] = CartesianCategory[:=>, :*:]
      with ApplicationCompose[:=>, :*:, :-->]
    type CoCartesianCategory[:=>[-_, +_], :+:[+_, +_]]                   = Category[:=>] with EitherCompose[:=>, :+:]
  }

}
