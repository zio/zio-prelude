package zio.prelude

trait IdExports {
  object Id extends NewtypeF
  type Id[+A] = Id.Type[A]
}
