package zio.prelude

import zio.prelude.Debug.{ Renderer, Repr }
import zio.prelude.Debug._
import zio.test._

object DebugSpec extends DefaultRunnableSpec {

  def primitiveTest[A: Debug](renderer: Renderer)(a: A, exp: Option[String] = None) =
    assert(a.debug.render(renderer))(equalTo(exp.getOrElse(a.toString)))

  def primScalaTest[A: Debug](a: A)                            = primitiveTest[A](Renderer.Scala)(a)
  def primFullTest[A: Debug](a: A, exp: Option[String] = None) = primitiveTest[A](Renderer.Full)(a, exp)

  case class TestCase(string: String, number: Int, list: List[Double])
  val genTestCase = for {
    str    <- Gen.anyString
    number <- Gen.anyInt
    list   <- Gen.listOf(Gen.anyDouble)
  } yield TestCase(str, number, list)

  implicit val DebugTestCase = Debug.make[TestCase](a =>
    Repr.Constructor(
      List("DebugSpec"),
      "TestCase",
      Map("string" -> a.string.debug, "number" -> a.number.debug, "list" -> a.list.debug)
    )
  )

  sealed trait TestTrait
  case object TestObject1 extends TestTrait
  case object TestObject2 extends TestTrait

  implicit val testObject: Debug[TestTrait] = _ match {
    case TestObject1 => Repr.Object(List("DebugSpec"), "TestObject1")
    case TestObject2 => Repr.Object(List("DebugSpec"), "TestObject2")
  }

  val genTestTrait = Gen.elements(List[TestTrait](TestObject1, TestObject2): _*)

  def expectedTupleFull(n: Int)(v: Int): String   = s"scala.Tuple$n(${List.fill(n)(v).mkString(", ")})"
  def expectedTupleSimple(n: Int)(v: Int): String = s"Tuple$n(${List.fill(n)(v).mkString(", ")})"

  def spec =
    suite("DebugSpec")(
      suite("ScalaRenderer")(
        testM("unit")(check(Gen.unit)(primScalaTest(_))),
        testM("int")(check(Gen.anyInt)(primFullTest(_))),
        testM("double")(check(Gen.anyDouble)(primScalaTest(_))),
        testM("float")(check(Gen.anyFloat)(primScalaTest(_))),
        testM("long")(check(Gen.anyLong)(primScalaTest(_))),
        testM("byte")(check(Gen.anyByte)(primScalaTest(_))),
        testM("char")(check(Gen.anyChar)(primScalaTest(_))),
        testM("string")(check(Gen.anyString)(primScalaTest(_))),
        testM("either")(check(Gen.either(Gen.anyString, Gen.anyInt))(primScalaTest(_))),
        testM("option")(check(Gen.option(Gen.anyInt))(primScalaTest(_))),
        testM("vector")(check(Gen.vectorOf(Gen.anyInt))(primScalaTest(_))),
        testM("map")(check(Gen.mapOf(Gen.anyInt, Gen.anyString))(primScalaTest(_))),
        testM("list")(check(Gen.listOf(Gen.anyInt))(primScalaTest(_))),
        testM("tuple2")(check(Gen.anyInt)(i => primScalaTest((i, i)))),
        testM("tuple3")(check(Gen.anyInt)(i => primScalaTest((i, i, i)))),
        testM("tuple4")(check(Gen.anyInt)(i => primScalaTest((i, i, i, i)))),
        testM("tuple10")(check(Gen.anyInt)(i => primScalaTest((i, i, i, i, i, i, i, i, i, i)))),
        testM("tuple22")(
          check(Gen.anyInt)(i => primScalaTest((i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i)))
        ),
        testM("caseclass")(check(genTestCase)(primScalaTest(_))),
        testM("testTrait")(check(genTestTrait)(primScalaTest(_)))
      ),
      suite("SimpleRenderer")(
        testM("list")(
          check(Gen.listOf(Gen.anyInt))(c => assert(c.debug.render(Renderer.Simple))(equalTo(c.toString)))
        ),
        testM("tuple2")(
          check(Gen.anyInt)(i => assert((i, i).debug.render(Renderer.Simple))(equalTo(expectedTupleSimple(2)(i))))
        ),
        testM("tuple3")(
          check(Gen.anyInt)(i => assert((i, i, i).debug.render(Renderer.Simple))(equalTo(expectedTupleSimple(3)(i))))
        ),
        testM("tuple4")(
          check(Gen.anyInt)(i => assert((i, i, i, i).debug.render(Renderer.Simple))(equalTo(expectedTupleSimple(4)(i))))
        ),
        testM("tuple10")(
          check(Gen.anyInt)(i =>
            assert((i, i, i, i, i, i, i, i, i, i).debug.render(Renderer.Simple))(equalTo(expectedTupleSimple(10)(i)))
          )
        ),
        testM("tuple22")(
          check(Gen.anyInt)(i =>
            assert((i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i).debug.render(Renderer.Simple))(
              equalTo(expectedTupleSimple(22)(i))
            )
          )
        ),
        testM("caseclass")(
          check(genTestCase)(c =>
            assert(c.debug.render(Renderer.Simple)) {
              val str = s""""${c.string}""""
              equalTo(s"TestCase(string -> $str, number -> ${c.number}, list -> ${c.list})")
            }
          )
        ),
        testM("testTrait")(check(genTestTrait) { c =>
          assert(c.debug.render(Renderer.Simple))(equalTo(s"${c.getClass.getSimpleName.init}"))
        })
      ),
      suite("FullRenderer")(
        testM("unit")(check(Gen.unit)(primFullTest(_, Some("scala.()")))),
        testM("int")(check(Gen.anyInt)(primFullTest(_))),
        testM("double")(check(Gen.anyDouble)(primFullTest(_))),
        testM("float")(check(Gen.anyFloat)(f => primFullTest(f, Some(s"${f.toString}f")))),
        testM("long")(check(Gen.anyLong)(l => primFullTest(l, Some(s"${l.toString}L")))),
        testM("byte")(check(Gen.anyByte)(primFullTest(_))),
        testM("char")(check(Gen.anyChar)(primFullTest(_))),
        testM("string")(check(Gen.anyString)(s => primFullTest(s, Some(s""""$s"""")))),
        testM("list")(
          check(Gen.listOf(Gen.anyInt))(c => assert(c.debug.render(Renderer.Full))(equalTo(s"scala.${c.toString}")))
        ),
        testM("tuple2")(
          check(Gen.anyInt)(i => assert((i, i).debug.render(Renderer.Full))(equalTo(expectedTupleFull(2)(i))))
        ),
        testM("tuple3")(
          check(Gen.anyInt)(i => assert((i, i, i).debug.render(Renderer.Full))(equalTo(expectedTupleFull(3)(i))))
        ),
        testM("tuple4")(
          check(Gen.anyInt)(i => assert((i, i, i, i).debug.render(Renderer.Full))(equalTo(expectedTupleFull(4)(i))))
        ),
        testM("tuple10")(
          check(Gen.anyInt)(i =>
            assert((i, i, i, i, i, i, i, i, i, i).debug.render(Renderer.Full))(equalTo(expectedTupleFull(10)(i)))
          )
        ),
        testM("tuple22")(
          check(Gen.anyInt)(i =>
            assert((i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i).debug.render(Renderer.Full))(
              equalTo(expectedTupleFull(22)(i))
            )
          )
        ),
        testM("caseclass")(
          check(genTestCase)(c =>
            assert(c.debug.render(Renderer.Full)) {
              val str = s""""${c.string}""""
              equalTo(s"DebugSpec.TestCase(string -> $str, number -> ${c.number}, list -> scala.${c.list})")
            }
          )
        ),
        testM("testTrait")(check(genTestTrait) { c =>
          assert(c.debug.render(Renderer.Full))(equalTo(s"DebugSpec.${c.getClass.getSimpleName.init}"))
        })
      )
    )
}
