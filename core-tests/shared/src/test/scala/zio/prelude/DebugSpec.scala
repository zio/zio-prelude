package zio.prelude

import zio.prelude.Debug.{Renderer, Repr, _}
import zio.prelude.laws._
import zio.test.{TestResult, _}
import zio.{Has, Random}

import scala.collection.immutable.ListMap

object DebugSpec extends DefaultRunnableSpec {

  def primitiveTest[A: Debug](renderer: Renderer)(a: A, exp: Option[String] = None): TestResult =
    assert(a.debug.render(renderer))(equalTo(exp.getOrElse(a.toString)))

  def primScalaTest[A: Debug](a: A, exp: Option[String] = None): TestResult  = primitiveTest[A](Renderer.Scala)(a, exp)
  def primSimpleTest[A: Debug](a: A, exp: Option[String] = None): TestResult = primitiveTest[A](Renderer.Simple)(a, exp)
  def primFullTest[A: Debug](a: A, exp: Option[String] = None): TestResult   = primitiveTest[A](Renderer.Full)(a, exp)

  final case class TestCase(string: String, number: Int, list: List[Double])
  val genTestCase: Gen[Has[Random] with Has[Sized], TestCase] = for {
    str    <- Gen.string
    number <- Gen.int
    list   <- Gen.listOf(Gen.double)
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

  val genTestTrait: Gen[Has[Random], TestTrait] = Gen.elements(List[TestTrait](TestObject1, TestObject2): _*)

  def expectedTupleFull(n: Int)(v: Int): String   = s"scala.Tuple$n(${List.fill(n)(v).mkString(", ")})"
  def expectedTupleSimple(n: Int)(v: Int): String = s"(${List.fill(n)(v).mkString(", ")})"

  def spec: ZSpec[Environment, Failure] =
    suite("DebugSpec")(
      suite("ScalaRenderer")(
        test("unit")(check(Gen.unit)(primScalaTest(_, Some("()")))),
        test("int")(check(Gen.int)(primFullTest(_))),
        test("double")(check(Gen.double)(primScalaTest(_))),
        test("float")(check(Gen.float)(primScalaTest(_))),
        test("long")(check(Gen.long)(primScalaTest(_))),
        test("byte")(check(Gen.byte)(primScalaTest(_))),
        test("char")(check(Gen.char)(primScalaTest(_))),
        test("short")(check(Gen.short)(primScalaTest(_))),
        test("string")(check(Gen.string)(primScalaTest(_))),
        test("either")(check(Gen.either(Gen.string, Gen.int))(primScalaTest(_))),
        test("option")(check(Gen.option(Gen.int))(primScalaTest(_))),
        test("vector")(check(Gen.vectorOf(Gen.int))(primScalaTest(_))),
        test("map")(
          check(Gen.mapOf(Gen.int, Gen.string))(c =>
            primitiveTest(Renderer.Scala)(c, Some(s"Map(${c.map(kv => s"${kv._1} -> ${kv._2}").mkString(", ")})"))
          )
        ),
        test("list")(check(Gen.listOf(Gen.int))(primScalaTest(_))),
        test("tuple2")(check(Gen.int)(i => primScalaTest((i, i)))),
        test("tuple3")(check(Gen.int)(i => primScalaTest((i, i, i)))),
        test("tuple4")(check(Gen.int)(i => primScalaTest((i, i, i, i)))),
        test("tuple10")(check(Gen.int)(i => primScalaTest((i, i, i, i, i, i, i, i, i, i)))),
        test("tuple22")(
          check(Gen.int)(i => primScalaTest((i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i)))
        ),
        test("caseclass")(check(genTestCase)(primScalaTest(_))),
        test("testTrait")(check(genTestTrait)(primScalaTest(_)))
      ),
      suite("SimpleRenderer")(
        test("unit")(check(Gen.unit)(primSimpleTest(_, Some("()")))),
        test("int")(check(Gen.int)(primSimpleTest(_))),
        test("double")(check(Gen.double)(primSimpleTest(_))),
        test("float")(check(Gen.float)(f => primSimpleTest(f, Some(s"${f.toString}f")))),
        test("long")(check(Gen.long)(l => primSimpleTest(l, Some(s"${l.toString}L")))),
        test("byte")(check(Gen.byte)(primSimpleTest(_))),
        test("char")(check(Gen.char)(s => primSimpleTest(s, Some(s"'$s'")))),
        test("short")(check(Gen.short)(primSimpleTest(_))),
        test("string")(check(Gen.string)(s => primSimpleTest(s, Some(s""""$s"""")))),
        test("either")(check(Gen.either(Gen.int, Gen.int))(primSimpleTest(_))),
        test("option")(check(Gen.option(Gen.int))(primSimpleTest(_))),
        test("vector")(check(Gen.vectorOf(Gen.int))(primSimpleTest(_))),
        test("map")(
          check(Gen.mapOf(Gen.int, Gen.int))(c =>
            primitiveTest(Renderer.Simple)(c, Some(s"Map(${c.map(kv => s"${kv._1} -> ${kv._2}").mkString(", ")})"))
          )
        ),
        test("list")(check(Gen.listOf(Gen.int))(primSimpleTest(_))),
        test("tuple2")(
          check(Gen.int)(i => assert((i, i).debug.render(Renderer.Simple))(equalTo(expectedTupleSimple(2)(i))))
        ),
        test("tuple3")(
          check(Gen.int)(i => assert((i, i, i).debug.render(Renderer.Simple))(equalTo(expectedTupleSimple(3)(i))))
        ),
        test("tuple4")(
          check(Gen.int)(i => assert((i, i, i, i).debug.render(Renderer.Simple))(equalTo(expectedTupleSimple(4)(i))))
        ),
        test("tuple10")(
          check(Gen.int)(i =>
            assert((i, i, i, i, i, i, i, i, i, i).debug.render(Renderer.Simple))(equalTo(expectedTupleSimple(10)(i)))
          )
        ),
        test("tuple22")(
          check(Gen.int)(i =>
            assert((i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i).debug.render(Renderer.Simple))(
              equalTo(expectedTupleSimple(22)(i))
            )
          )
        ),
        test("caseclass")(
          check(genTestCase)(c =>
            assert(c.debug.render(Renderer.Simple)) {
              val str = s""""${c.string}""""
              equalTo(s"TestCase(string = $str, number = ${c.number}, list = ${c.list})")
            }
          )
        ),
        test("testTrait")(check(genTestTrait) { c =>
          assert(c.debug.render(Renderer.Simple))(equalTo(s"${c.getClass.getSimpleName.init}"))
        })
      ),
      suite("FullRenderer")(
        test("unit")(check(Gen.unit)(primFullTest(_, Some("scala.()")))),
        test("int")(check(Gen.int)(primFullTest(_))),
        test("double")(check(Gen.double)(primFullTest(_))),
        test("float")(check(Gen.float)(f => primFullTest(f, Some(s"${f.toString}f")))),
        test("long")(check(Gen.long)(l => primFullTest(l, Some(s"${l.toString}L")))),
        test("byte")(check(Gen.byte)(primFullTest(_))),
        test("char")(check(Gen.char)(s => primFullTest(s, Some(s"'$s'")))),
        test("short")(check(Gen.short)(primFullTest(_))),
        test("string")(check(Gen.string)(s => primFullTest(s, Some(s""""$s"""")))),
        test("either")(
          check(Gen.either(Gen.int, Gen.int))(c =>
            assert(c.debug.render(Renderer.Full))(equalTo(s"scala.${c.toString}"))
          )
        ),
        test("option")(
          check(Gen.option(Gen.int))(c => assert(c.debug.render(Renderer.Full))(equalTo(s"scala.${c.toString}")))
        ),
        test("vector")(
          check(Gen.vectorOf(Gen.int))(c => assert(c.debug.render(Renderer.Full))(equalTo(s"scala.${c.toString}")))
        ),
        test("list")(
          check(Gen.listOf(Gen.int))(c => assert(c.debug.render(Renderer.Full))(equalTo(s"scala.${c.toString}")))
        ),
        test("map")(
          check(Gen.mapOf(Gen.int, Gen.int))((c: Map[Int, Int]) =>
            assert(c.debug.render(Renderer.Full))(
              equalTo(
                s"scala.Map(${c.map(kv => s"key: ${kv._1} -> value: ${kv._2}").mkString(", ")})"
              )
            )
          )
        ),
        test("tuple2")(
          check(Gen.int)(i => assert((i, i).debug.render(Renderer.Full))(equalTo(expectedTupleFull(2)(i))))
        ),
        test("tuple3")(
          check(Gen.int)(i => assert((i, i, i).debug.render(Renderer.Full))(equalTo(expectedTupleFull(3)(i))))
        ),
        test("tuple4")(
          check(Gen.int)(i => assert((i, i, i, i).debug.render(Renderer.Full))(equalTo(expectedTupleFull(4)(i))))
        ),
        test("tuple10")(
          check(Gen.int)(i =>
            assert((i, i, i, i, i, i, i, i, i, i).debug.render(Renderer.Full))(equalTo(expectedTupleFull(10)(i)))
          )
        ),
        test("tuple22")(
          check(Gen.int)(i =>
            assert((i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i).debug.render(Renderer.Full))(
              equalTo(expectedTupleFull(22)(i))
            )
          )
        ),
        test("caseclass")(
          check(genTestCase)(c =>
            assert(c.debug.render(Renderer.Full)) {
              val str = s""""${c.string}""""
              equalTo(s"DebugSpec.TestCase(string = $str, number = ${c.number}, list = scala.${c.list})")
            }
          )
        ),
        test("testTrait")(check(genTestTrait) { c =>
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
          import zio._
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
