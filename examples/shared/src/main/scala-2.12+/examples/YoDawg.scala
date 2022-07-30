package examples

import zio.prelude._

import java.util.UUID

object YoDawg {
  // I heard you like newtypes so I put a newtype in your newtype

  object GenericItemId extends Newtype[UUID]
  type GenericItemId = GenericItemId.Type

  object SpecificItemId extends Subtype[GenericItemId]
  type SpecificItemId = SpecificItemId.Type

  implicitly[GreatTypeclass[GenericItemId]]
  implicitly[GreatTypeclass[SpecificItemId]]

}

trait GreatTypeclass[-A] {
  def doSomething(a: A): String
}

object GreatTypeclass {
  import YoDawg.GenericItemId

  implicit val genericItemTypeclass: GreatTypeclass[GenericItemId] = new GreatTypeclass[GenericItemId] {
    def doSomething(a: GenericItemId): String = s"I am a generic item ${a}"
  }
}
