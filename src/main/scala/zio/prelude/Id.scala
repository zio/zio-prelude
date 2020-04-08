package zio.prelude

trait IdExports {
  object Id extends SubtypeF
  type Id[+A] = Id.Type[A]
}
