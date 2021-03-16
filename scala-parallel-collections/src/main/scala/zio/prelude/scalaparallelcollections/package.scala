package zio.prelude

package object scalaparallelcollections
    extends AssociativeFlattenInstances
    with SPCDerive
    with EqualInstances
    with HashInstances
    with InvariantInstances
    with OrdInstances
    with PartialOrdInstances
    with Syntax {}
