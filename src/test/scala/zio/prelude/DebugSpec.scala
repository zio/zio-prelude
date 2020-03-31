package zio.prelude

import zio.test._

object DebugSpec extends DefaultRunnableSpec {

  def primitiveTest[A: Debug](a: A, exp: Option[String] = None) = assert(a.debug.toString)(equalTo(exp.getOrElse(a.toString)))

  case class TestCase(string: String, number: Int, list: List[String])
  val genTestCase = for {
    str <- Gen.anyString
    number <- Gen.anyInt
    list <- Gen.listOf(Gen.anyString)
  } yield TestCase(str, number, list)

  def spec = suite("DebugSpec")(
    testM("unit")(check(Gen.unit)(primitiveTest(_, Some("scala.()")))),
    testM("int")(check(Gen.anyInt)(primitiveTest(_))),
    testM("double")(check(Gen.anyDouble)(primitiveTest(_))),
    testM("float")(check(Gen.anyFloat)(primitiveTest(_))),
    testM("long")(check(Gen.anyLong)(primitiveTest(_))),
    testM("byte")(check(Gen.anyByte)(primitiveTest(_))),
    testM("char")(check(Gen.anyChar)(primitiveTest(_))),
    testM("string")(check(Gen.anyString)(primitiveTest(_))),
    testM("list")(check(Gen.listOf(Gen.anyInt))(c => assert(c.debug.toString)(equalTo(c.toString))))
  )
}
