package zio.prelude

import zio.test._

object DebugSpec extends DefaultRunnableSpec {

  def primitiveTest[A: Debug](a: A, exp: Option[String] = None) =
    assert(a.debug.render())(equalTo(exp.getOrElse(a.toString)))

  case class TestCase(string: String, number: Int, list: List[String])
  val genTestCase = for {
    str    <- Gen.anyString
    number <- Gen.anyInt
    list   <- Gen.listOf(Gen.anyString)
  } yield TestCase(str, number, list)

  def spec = suite("DebugSpec")(
    testM("unit")(check(Gen.unit)(primitiveTest(_, Some("scala.()")))),
    testM("int")(check(Gen.anyInt)(primitiveTest(_))),
    testM("double")(check(Gen.anyDouble)(primitiveTest(_))),
    testM("float")(check(Gen.anyFloat)(f => primitiveTest(f, Some(s"${f.toString}f")))),
    testM("long")(check(Gen.anyLong)(l => primitiveTest(l, Some(s"${l.toString}L")))),
    testM("byte")(check(Gen.anyByte)(primitiveTest(_))),
    testM("char")(check(Gen.anyChar)(primitiveTest(_))),
    testM("string")(check(Gen.anyString)(s => primitiveTest(s, Some(s""""$s"""")))),
    testM("list")(check(Gen.listOf(Gen.anyInt))(c => assert(c.debug.render())(equalTo(s"scala.${c.toString}"))))
  )
}
