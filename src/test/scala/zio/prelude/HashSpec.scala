// package zio.prelude

// import zio.ZIO
// import zio.test._
// import zio.test.laws._
// import zio.test.Assertion._
// import zio.test.DefaultRunnableSpec
// import zio.prelude.Equal._

// object HashSpec extends DefaultRunnableSpec {
//   def spec = suite("HashSpec")(
//     suite("laws")(
//       testM("boolean")(checkAllLaws(Hash)(Gen.boolean)),
//       testM("byte")(checkAllLaws(Hash)(Gen.anyByte)),
//       testM("char")(checkAllLaws(Hash)(Gen.anyChar)),
//       testM("double")(checkAllLaws(Hash)(Gen.anyDouble)),
//       testM("either")(checkAllLaws(Hash)(Gen.either(Gen.anyInt, Gen.anyInt))),
//       testM("float")(checkAllLaws(Hash)(Gen.anyFloat)),
//       testM("int")(checkAllLaws(Hash)(Gen.anyInt)),
//       testM("list")(checkAllLaws(Hash)(Gen.listOf(Gen.anyInt))),
//       testM("long")(checkAllLaws(Hash)(Gen.anyLong)),
//       testM("map")(checkAllLaws(Hash)(Gen.mapOf(Gen.anyInt, Gen.anyInt))),
//       testM("option")(checkAllLaws(Hash)(Gen.option(Gen.anyInt))),
//       testM("set")(checkAllLaws(Hash)(Gen.setOf(Gen.anyInt))),
//       testM("string")(checkAllLaws(Hash)(Gen.anyString)),
//       testM("tuple2")(checkAllLaws(Hash)(Gen.anyInt.zip(Gen.anyString))),
//       testM("unit")(checkAllLaws(Hash)(Gen.unit)),
//       testM("vector")(checkAllLaws(Hash)(Gen.vectorOf(Gen.anyInt)))
//       )
//     )
// }
