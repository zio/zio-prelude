package zio.prelude

import zio.prelude.Debug.{Renderer, Repr, _}
import zio.prelude.laws._
import zio.random.Random
import zio.test.{TestResult, _}

import scala.collection.immutable.ListMap

object DebugSpec extends DefaultRunnableSpec {

  def primitiveTest[A: Debug](renderer: Renderer)(a: A, exp: Option[String] = None): TestResult =
    assert(a.debug.render(renderer))(equalTo(exp.getOrElse(a.toString)))

  def primScalaTest[A: Debug](a: A, exp: Option[String] = None): TestResult  = primitiveTest[A](Renderer.Scala)(a, exp)
  def primSimpleTest[A: Debug](a: A, exp: Option[String] = None): TestResult = primitiveTest[A](Renderer.Simple)(a, exp)
  def primFullTest[A: Debug](a: A, exp: Option[String] = None): TestResult   = primitiveTest[A](Renderer.Full)(a, exp)

  final case class TestCase(string: String, number: Int, list: List[Double])
  val genTestCase: Gen[Random with Sized, TestCase] = for {
    str    <- Gen.anyString
    number <- Gen.anyInt
    list   <- Gen.listOf(Gen.anyDouble)
  } yield TestCase(str, number, list)

  implicit val DebugTestCase: Debug[TestCase] = Debug.make[TestCase](a =>
    Repr.Constructor(
      List("DebugSpec"),
      "TestCase",
      ListMap("string" -> a.string.debug, "number" -> a.number.debug, "list" -> a.list.debug)
    )
  )

  sealed trait TestTrait
  case object TestObject1 extends TestTrait
  case object TestObject2 extends TestTrait

  implicit val testObject: Debug[TestTrait] = {
    case TestObject1 => Repr.Object(List("DebugSpec"), "TestObject1")
    case TestObject2 => Repr.Object(List("DebugSpec"), "TestObject2")
  }

  val genTestTrait: Gen[Random, TestTrait] = Gen.elements(List[TestTrait](TestObject1, TestObject2): _*)

  def expectedTupleFull(n: Int)(v: Int): String   = s"scala.Tuple$n(${List.fill(n)(v).mkString(", ")})"
  def expectedTupleSimple(n: Int)(v: Int): String = s"(${List.fill(n)(v).mkString(", ")})"

  def spec: ZSpec[Environment, Failure] =
    suite("DebugSpec")(
      suite("ScalaRenderer")(
        testM("unit")(check(Gen.unit)(primScalaTest(_, Some("()")))),
        testM("int")(check(Gen.anyInt)(primFullTest(_))),
        testM("double")(check(Gen.anyDouble)(primScalaTest(_))),
        testM("float")(check(Gen.anyFloat)(primScalaTest(_))),
        testM("long")(check(Gen.anyLong)(primScalaTest(_))),
        testM("byte")(check(Gen.anyByte)(primScalaTest(_))),
        testM("char")(check(Gen.anyChar)(primScalaTest(_))),
        testM("short")(check(Gen.anyShort)(primScalaTest(_))),
        testM("string")(check(Gen.anyString)(primScalaTest(_))),
        testM("either")(check(Gen.either(Gen.anyString, Gen.anyInt))(primScalaTest(_))),
        testM("option")(check(Gen.option(Gen.anyInt))(primScalaTest(_))),
        testM("vector")(check(Gen.vectorOf(Gen.anyInt))(primScalaTest(_))),
        testM("map")(
          check(Gen.mapOf(Gen.anyInt, Gen.anyString))(c =>
            primitiveTest(Renderer.Scala)(c, Some(s"Map(${c.map(kv => s"${kv._1} -> ${kv._2}").mkString(", ")})"))
          )
        ),
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
        testM("unit")(check(Gen.unit)(primSimpleTest(_, Some("()")))),
        testM("int")(check(Gen.anyInt)(primSimpleTest(_))),
        testM("double")(check(Gen.anyDouble)(primSimpleTest(_))),
        testM("float")(check(Gen.anyFloat)(f => primSimpleTest(f, Some(s"${f.toString}f")))),
        testM("long")(check(Gen.anyLong)(l => primSimpleTest(l, Some(s"${l.toString}L")))),
        testM("byte")(check(Gen.anyByte)(primSimpleTest(_))),
        testM("char")(check(Gen.anyChar)(s => primSimpleTest(s, Some(s"'$s'")))),
        testM("short")(check(Gen.anyShort)(primSimpleTest(_))),
        testM("string")(check(Gen.anyString)(s => primSimpleTest(s, Some(s""""$s"""")))),
        testM("either")(check(Gen.either(Gen.anyInt, Gen.anyInt))(primSimpleTest(_))),
        testM("option")(check(Gen.option(Gen.anyInt))(primSimpleTest(_))),
        testM("vector")(check(Gen.vectorOf(Gen.anyInt))(primSimpleTest(_))),
        testM("map")(
          check(Gen.mapOf(Gen.anyInt, Gen.anyInt))(c =>
            primitiveTest(Renderer.Simple)(c, Some(s"Map(${c.map(kv => s"${kv._1} -> ${kv._2}").mkString(", ")})"))
          )
        ),
        testM("list")(check(Gen.listOf(Gen.anyInt))(primSimpleTest(_))),
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
              equalTo(s"TestCase(string = $str, number = ${c.number}, list = ${c.list})")
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
        testM("char")(check(Gen.anyChar)(s => primFullTest(s, Some(s"'$s'")))),
        testM("short")(check(Gen.anyShort)(primFullTest(_))),
        testM("string")(check(Gen.anyString)(s => primFullTest(s, Some(s""""$s"""")))),
        testM("either")(
          check(Gen.either(Gen.anyInt, Gen.anyInt))(c =>
            assert(c.debug.render(Renderer.Full))(equalTo(s"scala.${c.toString}"))
          )
        ),
        testM("option")(
          check(Gen.option(Gen.anyInt))(c => assert(c.debug.render(Renderer.Full))(equalTo(s"scala.${c.toString}")))
        ),
        testM("vector")(
          check(Gen.vectorOf(Gen.anyInt))(c => assert(c.debug.render(Renderer.Full))(equalTo(s"scala.${c.toString}")))
        ),
        testM("list")(
          check(Gen.listOf(Gen.anyInt))(c => assert(c.debug.render(Renderer.Full))(equalTo(s"scala.${c.toString}")))
        ),
        testM("map")(
          check(Gen.mapOf(Gen.anyInt, Gen.anyInt))((c: Map[Int, Int]) =>
            assert(c.debug.render(Renderer.Full))(
              equalTo(
                s"scala.Map(${c.map(kv => s"key: ${kv._1} -> value: ${kv._2}").mkString(", ")})"
              )
            )
          )
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
              equalTo(s"DebugSpec.TestCase(string = $str, number = ${c.number}, list = scala.${c.list})")
            }
          )
        ),
        testM("testTrait")(check(genTestTrait) { c =>
          assert(c.debug.render(Renderer.Full))(equalTo(s"DebugSpec.${c.getClass.getSimpleName.init}"))
        })
      ),
      suite("DebugInterpolator")(
        test("String") {
          val name = "World"
          assert(d"Hello, my name is $name.")(equalTo("Hello, my name is \"World\"."))
        },
        test("Int") {
          val sum = 1 + 1
          assert(d"1 + 1 is $sum.")(equalTo("1 + 1 is 2."))
        }
      ),
      suite("Duration")(
        test("ZIO") {
          import zio.duration._
          val duration = 3.millis
          assert(duration.debug.render)(equalTo("Duration(amount = 3L, unit = MILLISECONDS)"))
        },
        test("Scala") {
          import scala.concurrent.duration._
          val duration = 42.days
          assert(duration.debug.render)(equalTo("Duration(length = 42L, unit = DAYS)"))
        }
      )
    )
}
