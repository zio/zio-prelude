package zio.prelude.fx

import java.util.NoSuchElementException

import zio.prelude._
import zio.random.Random
import zio.test.Assertion.{ anything, isLeft, isNone, isRight, isSome, isSubtype }
import zio.test._
import zio.{ CanFail, Chunk }

import scala.util.Try

object ZPureSpec extends DefaultRunnableSpec {

  lazy val genInt: Gen[Random, Int] =
    Gen.anyInt

  lazy val genString: Gen[Random with Sized, String] =
    Gen.anyString

  lazy val genIntIntToInt: Gen[Random, (Int, Int) => Int] =
    Gen.function2(genInt)

  lazy val genIntToInt: Gen[Random, Int => Int] =
    Gen.function(genInt)

  lazy val genIntToIntInt: Gen[Random, Int => (Int, Int)] =
    Gen.function(genInt <*> genInt)

  lazy val genIntToState: Gen[Random, Int => State[Int, Int]] =
    Gen.function(genState)

  lazy val genState: Gen[Random, State[Int, Int]] =
    Gens.state(genInt, genInt)

  lazy val genStateState: Gen[Random, State[Int, State[Int, Int]]] =
    Gens.state(genInt, genState)

  def spec: ZSpec[Environment, Failure] =
    suite("ZPureSpec")(
      suite("context")(
        suite("constructors")(
          testM("access") {
            check(genIntToInt, genInt, genInt) { (f, r, s) =>
              val actual   = ZPure.access(f).provide(r).run(s)
              val expected = (s, f(r))
              assert(actual)(equalTo(expected))
            }
          },
          test("accessM") {
            val zPure = ZPure.accessM[Int](n => State.update[Int, Int](_ + n))
            assert(zPure.provide(2).runState(3))(equalTo(5))
          },
          test("provide is scoped correctly") {
            val zPure = for {
              start <- ZPure.environment[Any, Int]
              inner <- (for {
                         innerStart <- ZPure.environment[Any, Int]
                         innerInner <- ZPure.environment[Any, Int].provide(111)
                         innerEnd   <- ZPure.environment[Any, Int]
                       } yield (innerStart, innerInner, innerEnd)).provide(11)
              end   <- ZPure.environment[Any, Int]
            } yield (start, inner, end)
            assert(zPure.provide(1).runResult(()))(equalTo((1, (11, 111, 11), 1)))
          },
          test("provided environment should be restored on error") {
            val zPure = for {
              _   <- (ZPure.fail(()): ZPure[Nothing, Any, Any, Int, Unit, Nothing]).provide(1).either
              end <- ZPure.environment[Any, Int]
            } yield end
            assert(zPure.provide(0).runResult(()))(equalTo(0))
          }
        )
      ),
      suite("state")(
        suite("methods")(
          testM("|||") {
            check(genInt, genInt, genInt, genInt, genInt) { (s0, s1, s2, a1, a2) =>
              val z1 = ZPure.fromFunction[Int, Unit, Int](_ => a1).asState(s1)
              val z2 = ZPure.fromFunction[Int, Unit, Int](_ => a2).asState(s2)
              assert((z1 ||| z2).provide(Left(())).run(s0))(equalTo((s1, a1))) &&
              assert((z1 ||| z2).provide(Right(())).run(s0))(equalTo((s2, a2)))
            }
          },
          testM("contramap") {
            check(genState, genIntToInt, genInt) { (fa, f, s) =>
              val (s1, a1) = fa.run(s)
              assert(fa.mapState(f).run(s))(equalTo((f(s1), a1)))
            }
          },
          testM("filterOrElse") {
            check(genInt, genInt, genInt, genInt, genInt) { (s1, s2, s3, a1, a2) =>
              val z = ZPure.succeed[Int, Int](a1).asState(s2)
              val f = (_: Int) => ZPure.succeed(a2).asState(s3)
              assert(z.filterOrElse(_ => true)(f).run(s1))(equalTo((s2, a1))) &&
              assert(z.filterOrElse(_ => false)(f).run(s1))(equalTo((s3, a2)))
            }
          },
          testM("filterOrElse_") {
            check(genInt, genInt, genInt, genInt, genInt) { (s1, s2, s3, a1, a2) =>
              val z1 = ZPure.succeed[Int, Int](a1).asState(s2)
              val z2 = ZPure.succeed(a2).asState(s3)
              assert(z1.filterOrElse_(_ => true)(z2).run(s1))(equalTo((s2, a1))) &&
              assert(z1.filterOrElse_(_ => false)(z2).run(s1))(equalTo((s3, a2)))
            }
          },
          testM("filterOrFail") {
            check(genInt, genInt) { (a, e) =>
              val z = ZPure.succeed[Unit, Int](a)
              assert(z.filterOrFail(_ => true)(e).runEither(()))(isRight(equalTo(((), a)))) &&
              assert(z.filterOrFail(_ => false)(e).runEither(()))(isLeft(equalTo(e)))
            }
          },
          testM("flatMap") {
            check(genState, genIntToState, genInt) { (fa, f, s) =>
              val (s1, a1) = fa.run(s)
              val (s2, a2) = f(a1).run(s1)
              assert(fa.flatMap(f).run(s))(equalTo((s2, a2)))
            }
          },
          testM("flatten") {
            check(genStateState, genInt) { (ffa, s) =>
              val (s1, fa) = ffa.run(s)
              val (s2, b)  = fa.run(s1)
              assert(ffa.flatten.run(s))(equalTo((s2, b)))
            }
          },
          testM("head") {
            check(genInt, genString, genString) { (s, el, el2) =>
              val optOrHead = ZPure.succeed[Int, List[String]](List(el, el2)).head.runEither(s)
              assert(optOrHead)(isRight(equalTo((s, el))))
            }
          },
          testM("head (Failure case)") {
            check(genInt, genString, genString) { (s, e, el) =>
              val optOrHead = ZPure.fail(e).as(List(el)).head.runEither(s)
              assert(optOrHead)(isLeft(equalTo(Option(e))))
            }
          },
          testM("head (empty List)") {
            check(genInt) { s =>
              val optOrHead = ZPure.succeed[Int, List[String]](List.empty).head.runEither(s)
              assert(optOrHead)(isLeft(equalTo(Option.empty[String])))
            }
          },
          testM("join") {
            check(genInt, genInt, genInt, genInt, genInt) { (s0, s1, s2, a1, a2) =>
              val z1 = ZPure.fromFunction[Int, Unit, Int](_ => a1).asState(s1)
              val z2 = ZPure.fromFunction[Int, Unit, Int](_ => a2).asState(s2)
              assert(z1.join(z2).provide(Left(())).run(s0))(equalTo((s1, a1))) &&
              assert(z1.join(z2).provide(Right(())).run(s0))(equalTo((s2, a2)))
            }
          },
          testM("map") {
            check(genState, genIntToInt, genInt) { (fa, f, s) =>
              val (s1, a1) = fa.run(s)
              assert(fa.map(f).run(s))(equalTo((s1, f(a1))))
            }
          },
          testM("mapState") {
            check(genState, genIntToInt, genInt) { (fa, f, s) =>
              val (s1, a1) = fa.run(s)
              assert(fa.mapState(f).run(s))(equalTo((f(s1), a1)))
            }
          },
          testM("negate") {
            check(genInt) { s =>
              assert(State.succeed[Int, Boolean](true).negate.run(s))(equalTo((s, false))) &&
              assert(State.succeed[Int, Boolean](false).negate.run(s))(equalTo((s, true)))
            }
          },
          suite("repeatN")(
            test("success") {
              val f = (s: Int) => (s + 1, (s + 1) * 10)
              assert(State.modify(f).repeatN(0).run(0))(equalTo((1, 10))) &&
              assert(State.modify(f).repeatN(1).run(0))(equalTo((2, 20))) &&
              assert(State.modify(f).repeatN(2).run(0))(equalTo((3, 30))) &&
              assert(State.modify(f).repeatN(3).run(0))(equalTo((4, 40))) &&
              assert(State.modify(f).repeatN(4).run(0))(equalTo((5, 50)))
            },
            test("failure") {
              val f = (s: Int) => if (s == 3) Left("error") else Right((s + 1, (s + 1) * 10))
              assert(ZPure.modifyEither(f).repeatN(5).runEither(0))(isLeft(equalTo("error")))
            }
          ),
          suite("repeatUntil")(
            test("success") {
              val f = (s: Int) => (s + 1, (s + 1) / 10)
              assert(State.modify(f).repeatUntil(_ == 0).run(0))(equalTo((1, 0))) &&
              assert(State.modify(f).repeatUntil(_ == 1).run(0))(equalTo((10, 1))) &&
              assert(State.modify(f).repeatUntil(_ == 2).run(0))(equalTo((20, 2))) &&
              assert(State.modify(f).repeatUntil(_ == 3).run(0))(equalTo((30, 3))) &&
              assert(State.modify(f).repeatUntil(_ == 4).run(0))(equalTo((40, 4)))
            },
            test("failure") {
              val f = (s: Int) => if (s == 3) Left("error") else Right((s + 1, (s + 1) / 10))
              assert(ZPure.modifyEither(f).repeatUntil(_ == 1).runEither(0))(isLeft(equalTo("error")))
            }
          ),
          suite("repeatUntilEquals")(
            test("success") {
              val f = (s: Int) => (s + 1, (s + 1) / 10)
              assert(State.modify(f).repeatUntilEquals(0).run(0))(equalTo((1, 0))) &&
              assert(State.modify(f).repeatUntilEquals(1).run(0))(equalTo((10, 1))) &&
              assert(State.modify(f).repeatUntilEquals(2).run(0))(equalTo((20, 2))) &&
              assert(State.modify(f).repeatUntilEquals(3).run(0))(equalTo((30, 3))) &&
              assert(State.modify(f).repeatUntilEquals(4).run(0))(equalTo((40, 4)))
            },
            test("failure") {
              val f = (s: Int) => if (s == 3) Left("error") else Right((s + 1, (s + 1) / 10))
              assert(ZPure.modifyEither(f).repeatUntilEquals(1).runEither(0))(isLeft(equalTo("error")))
            }
          ),
          suite("repeatUntilState")(
            test("success") {
              val f = (s: Int) => (s + 1, (s + 1) / 10)
              assert(State.modify(f).repeatUntilState(_ == 1).run(0))(equalTo((1, 0))) &&
              assert(State.modify(f).repeatUntilState(_ == 10).run(0))(equalTo((10, 1))) &&
              assert(State.modify(f).repeatUntilState(_ == 20).run(0))(equalTo((20, 2))) &&
              assert(State.modify(f).repeatUntilState(_ == 30).run(0))(equalTo((30, 3))) &&
              assert(State.modify(f).repeatUntilState(_ == 40).run(0))(equalTo((40, 4)))
            },
            test("failure") {
              val f = (s: Int) => if (s == 3) Left("error") else Right((s + 1, (s + 1) / 10))
              assert(ZPure.modifyEither(f).repeatUntilState(_ == 10).runEither(0))(isLeft(equalTo("error")))
            }
          ),
          suite("repeatUntilStateEquals")(
            test("success") {
              val f = (s: Int) => (s + 1, (s + 1) / 10)
              assert(State.modify(f).repeatUntilStateEquals(1).run(0))(equalTo((1, 0))) &&
              assert(State.modify(f).repeatUntilStateEquals(10).run(0))(equalTo((10, 1))) &&
              assert(State.modify(f).repeatUntilStateEquals(20).run(0))(equalTo((20, 2))) &&
              assert(State.modify(f).repeatUntilStateEquals(30).run(0))(equalTo((30, 3))) &&
              assert(State.modify(f).repeatUntilStateEquals(40).run(0))(equalTo((40, 4)))
            },
            test("failure") {
              val f = (s: Int) => if (s == 3) Left("error") else Right((s + 1, (s + 1) / 10))
              assert(ZPure.modifyEither(f).repeatUntilStateEquals(10).runEither(0))(isLeft(equalTo("error")))
            }
          ),
          suite("repeatWhile")(
            test("success") {
              val f = (s: Int) => (s + 1, (s + 1) / 10)
              assert(State.modify(f).repeatWhile(_ < 0).run(0))(equalTo((1, 0))) &&
              assert(State.modify(f).repeatWhile(_ < 1).run(0))(equalTo((10, 1))) &&
              assert(State.modify(f).repeatWhile(_ < 2).run(0))(equalTo((20, 2))) &&
              assert(State.modify(f).repeatWhile(_ < 3).run(0))(equalTo((30, 3))) &&
              assert(State.modify(f).repeatWhile(_ < 4).run(0))(equalTo((40, 4)))
            },
            test("failure") {
              val f = (s: Int) => if (s == 3) Left("error") else Right((s + 1, (s + 1) / 10))
              assert(ZPure.modifyEither(f).repeatWhile(_ < 1).runEither(0))(isLeft(equalTo("error")))
            }
          ),
          suite("repeatWhileEquals")(
            test("success") {
              val f = (s: Int) => (s + 1, (s + 1) / 10)
              assert(State.modify(f).repeatWhileEquals(0).run(0))(equalTo((10, 1)))
            },
            test("failure") {
              val f = (s: Int) => if (s == 3) Left("error") else Right((s + 1, (s + 1) / 10))
              assert(ZPure.modifyEither(f).repeatWhileEquals(0).runEither(0))(isLeft(equalTo("error")))
            }
          ),
          suite("repeatWhileState")(
            test("success") {
              val f = (s: Int) => (s + 1, (s + 1) / 10)
              assert(State.modify(f).repeatWhileState(_ < 1).run(0))(equalTo((1, 0))) &&
              assert(State.modify(f).repeatWhileState(_ < 10).run(0))(equalTo((10, 1))) &&
              assert(State.modify(f).repeatWhileState(_ < 20).run(0))(equalTo((20, 2))) &&
              assert(State.modify(f).repeatWhileState(_ < 30).run(0))(equalTo((30, 3))) &&
              assert(State.modify(f).repeatWhileState(_ < 40).run(0))(equalTo((40, 4)))
            },
            test("failure") {
              val f = (s: Int) => if (s == 3) Left("error") else Right((s + 1, (s + 1) / 10))
              assert(ZPure.modifyEither(f).repeatWhileState(_ < 10).runEither(0))(isLeft(equalTo("error")))
            }
          ),
          testM("run") {
            check(genIntToIntInt, genInt) { (f, s) =>
              assert(State.modify(f).run(s))(equalTo(f(s)))
            }
          },
          testM("runResult") {
            check(genIntToIntInt, genInt) { (f, s) =>
              assert(State.modify(f).runResult(s))(equalTo(f(s)._2))
            }
          },
          testM("runState") {
            check(genIntToIntInt, genInt) { (f, s) =>
              assert(State.modify(f).runState(s))(equalTo(f(s)._1))
            }
          },
          testM("unit") {
            check(genInt, genInt) { (s, a) =>
              assert(State.succeed[Int, Int](a).unit.run(s))(equalTo((s, ())))
            }
          },
          testM("zip") {
            check(genState, genState, genInt) { (fa, fb, s) =>
              val (s1, a) = fa.run(s)
              val (s2, b) = fb.run(s1)
              assert(fa.zip(fb).run(s))(equalTo((s2, (a, b))))
            }
          },
          testM("zipLeft") {
            check(genState, genState, genInt) { (fa, fb, s) =>
              val (s1, a) = fa.run(s)
              val (s2, _) = fb.run(s1)
              assert(fa.zipLeft(fb).run(s))(equalTo((s2, a)))
            }
          },
          testM("zipRight") {
            check(genState, genState, genInt) { (fa, fb, s) =>
              val (s1, _) = fa.run(s)
              val (s2, b) = fb.run(s1)
              assert(fa.zipRight(fb).run(s))(equalTo((s2, b)))
            }
          },
          testM("zipWith") {
            check(genState, genState, genIntIntToInt, genInt) { (fa, fb, f, s) =>
              val (s1, a) = fa.run(s)
              val (s2, b) = fb.run(s1)
              assert(fa.zipWith(fb)(f).run(s))(equalTo((s2, f(a, b))))
            }
          }
        ),
        suite("constructors")(
          testM("get") {
            check(genInt) { s =>
              assert(State.get.run(s))(equalTo((s, s)))
            }
          },
          testM("modify") {
            check(Gen.anyInt, genIntToIntInt) { (s, f) =>
              assert(State.modify(f).run(s))(equalTo(f(s)))
            }
          },
          testM("set") {
            check(genInt, genInt) { (s1, s2) =>
              assert(State.set(s2).run(s1))(equalTo((s2, ())))
            }
          },
          testM("asState") {
            check(genInt, genInt, genInt) { (s1, s2, s3) =>
              assert(State.set(s2).asState(s3).run(s1))(equalTo((s3, ())))
            }
          },
          testM("succeed") {
            check(genInt, genInt) { (s, a) =>
              assert(State.succeed(a).run(s))(equalTo((s, a)))
            }
          },
          testM("unit") {
            check(genInt) { s =>
              assert(State.unit.run(s))(equalTo((s, ())))
            }
          },
          testM("update") {
            check(genInt, genIntToInt) { (s, f) =>
              assert(State.update(f).run(s))(equalTo((f(s), ())))
            }
          }
        )
      ),
      suite("failure")(
        suite("methods")(
          testM("either") {
            check(genInt, genInt) { (s1, e) =>
              val (s2, a) = ZPure.fail(e).either.run(s1)
              assert(s2)(equalTo(s1)) && assert(a)(isLeft(equalTo(e)))
            }
          },
          suite("none")(
            testM("success") {
              check(genInt) { s =>
                assert(ZPure.succeed[Int, Option[Int]](None).none.runEither(s))(isRight(equalTo((s, ()))))
              }
            },
            testM("failure") {
              check(genInt, genInt) { (s, a) =>
                assert(ZPure.succeed[Int, Option[Int]](Some(a)).none.runEither(s))(isLeft(isNone))
              }
            }
          ),
          testM("orElseFail") {
            check(genInt, genInt, genString) { (s1, e, e1) =>
              val errorOrUpdate = ZPure.fail(e).orElseFail(e1).runEither(s1)
              assert(errorOrUpdate)(isLeft(equalTo(e1)))
            }
          },
          testM("orElseOptional (Some case)") {
            check(genInt, genString, genString) { (s1, e, e1) =>
              val errorOrUpdate = ZPure.fail(Some(e)).orElseOptional(ZPure.fail(Some(e1))).runEither(s1)
              assert(errorOrUpdate)(isLeft(equalTo(Option(e))))
            }
          },
          testM("orElseOptional (None case)") {
            check(genInt, genString) { (s1, e) =>
              val errorOrUpdate = ZPure.fail(Option.empty[String]).orElseOptional(ZPure.fail(Some(e))).runEither(s1)
              assert(errorOrUpdate)(isLeft(equalTo(Option(e))))
            }
          },
          testM("orElseSucceed (Success case)") {
            implicit val canFail = CanFail
            check(genInt, genInt, genInt) { (s1, v, v1) =>
              val (_, a) = ZPure.succeed(v).orElseSucceed(v1).run(s1)
              assert(a)(equalTo(v))
            }
          },
          testM("orElseSucceed (Failure case)") {
            check(genInt, genString, genInt) { (s1, e, v1) =>
              val (_, a) = ZPure.fail(e).orElseSucceed(v1).run(s1)
              assert(a)(equalTo(v1))
            }
          },
          testM("orElseFallback (Success case)") {
            implicit val canFail = CanFail
            check(genInt, genInt, genInt, genInt) { (s1, s3, v, v1) =>
              val (s, a) = ZPure.succeed[Int, Int](v).orElseFallback(v1, s3).run(s1)
              assert(a)(equalTo(v)) && assert(s)(equalTo(s1))
            }
          },
          testM("orElseFallback (Failure case)") {
            check(genInt, genInt, genString, genInt) { (s1, s3, e, v1) =>
              val (s, a) = ZPure.fail(e).orElseFallback(v1, s3).run(s1)
              assert(a)(equalTo(v1)) && assert(s)(equalTo(s3))
            }
          },
          suite("fold")(
            testM("failure") {
              check(genInt, genInt, genIntToInt, genIntToInt) { (s1, e, failure, success) =>
                val (s2, a) = ZPure.fail(e).fold(failure, success).run(s1)
                assert(s2)(equalTo(s1)) && assert(a)(equalTo(failure(e)))
              }
            },
            testM("success") {
              implicit val canFail = CanFail
              check(genInt, genInt, genIntToInt, genIntToInt) { (s1, a1, failure, success) =>
                val (s2, a2) = ZPure.succeed[Int, Int](a1).fold(failure, success).run(s1)
                assert(s2)(equalTo(s1)) && assert(a2)(equalTo(success(a1)))
              }
            }
          ),
          suite("foldM")(
            test("failure") {
              implicit val canFail = CanFail
              val failing          =
                ZPure.succeed[Int, Int](1).flatMap(n => if (n % 2 !== 0) ZPure.fail("fail") else ZPure.succeed(n))
              val result           = failing.foldM(
                _ => State.update[Int, Int](_ + 1) *> ZPure.succeed(0),
                a => State.update[Int, Int](_ + 2) *> ZPure.succeed(a)
              )
              assert(result.run(10))(equalTo((11, 0)))
            },
            test("success") {
              implicit val canFail = CanFail
              val failing          =
                ZPure.succeed[Int, Int](2).flatMap(n => if (n % 2 !== 0) ZPure.fail("fail") else ZPure.succeed(n))
              val result           = failing.foldM(
                _ => State.update[Int, Int](_ + 1) *> ZPure.succeed(0),
                a => State.update[Int, Int](_ + 2) *> ZPure.succeed(a)
              )
              assert(result.run(10))(equalTo((12, 2)))
            }
          ),
          suite("left methods")(
            suite("left")(
              test("failure") {
                val result = ZPure.fail("fail").left
                assert(result.runEither(0))(isLeft(isSome(equalTo("fail"))))
              },
              test("right") {
                val result = ZPure.succeed[Int, Either[Nothing, Int]](Right(1)).left
                assert(result.runEither(0))(isLeft(isNone))
              },
              test("left") {
                val result = ZPure.succeed[Int, Either[String, Int]](Left("Left")).left
                assert(result.runEither(0))(isRight(equalTo((0, "Left"))))
              }
            ),
            suite("leftOrFail")(
              test("failure") {
                val result = ZPure.fail("fail").leftOrFail("oh crap")
                assert(result.runEither(0))(isLeft(equalTo("fail")))
              },
              test("right") {
                val result = ZPure
                  .succeed[Int, Either[Nothing, Int]](Right(1))
                  .leftOrFail("oh crap")
                assert(result.runEither(0))(isLeft(equalTo("oh crap")))
              },
              test("left") {
                val result = ZPure.succeed[Int, Either[String, Int]](Left("Left")).leftOrFail("oh crap")
                assert(result.runEither(0))(isRight(equalTo((0, "Left"))))
              }
            ),
            suite("leftOrFailWith")(
              test("failure") {
                val result = ZPure.fail("fail").leftOrFailWith[Any, Any, String](_ => "Oh crap")
                assert(result.runEither(0))(isLeft(equalTo("fail")))
              },
              test("right") {
                val result = ZPure
                  .succeed[Int, Either[Nothing, Int]](Right(1))
                  .leftOrFailWith[Any, Any, String](_ => "oh crap")
                assert(result.runEither(0))(isLeft(equalTo("oh crap")))
              },
              test("left") {
                val result = ZPure.succeed[Int, Either[String, Int]](Left("Left")).leftOrFail("oh crap")
                assert(result.runEither(0))(isRight(equalTo((0, "Left"))))
              }
            ),
            suite("leftOrFailWithException")(
              test("failure") {
                val result = ZPure.fail(new NoSuchElementException()).leftOrFailWithException
                assert(result.runEither(0))(isLeft(isSubtype[NoSuchElementException](anything)))
              },
              test("right") {
                val result = ZPure.succeed[Int, Either[Nothing, Int]](Right(1)).leftOrFailWithException
                assert(result.runEither(0))(isLeft(isSubtype[NoSuchElementException](anything)))
              },
              test("left") {
                val result = ZPure.succeed[Int, Either[String, Int]](Left("Left")).leftOrFailWithException
                assert(result.runEither(0))(isRight(equalTo((0, "Left"))))
              }
            )
          ),
          suite("right methods")(
            suite("right")(
              test("failure") {
                val result = ZPure.fail("fail").right
                assert(result.runEither(0))(isLeft(isSome(equalTo("fail"))))
              },
              test("right") {
                val result = ZPure.succeed[Int, Either[Nothing, String]](Right("Right")).right
                assert(result.runEither(0))(isRight(equalTo((0, "Right"))))
              },
              test("left") {
                val result = ZPure.succeed[Int, Either[Int, Nothing]](Left(1)).right
                assert(result.runEither(0))(isLeft(isNone))
              }
            ),
            suite("rightOrFail")(
              test("failure") {
                val result = ZPure.fail("fail").rightOrFail("oh crap")
                assert(result.runEither(0))(isLeft(equalTo("fail")))
              },
              test("right") {
                val result = ZPure
                  .succeed[Int, Either[Nothing, Int]](Right(1))
                  .rightOrFail("oh crap")
                assert(result.runEither(0))(isRight(equalTo((0, 1))))
              },
              test("left") {
                val result = ZPure.succeed[Int, Either[String, Int]](Left("Left")).rightOrFail("oh crap")
                assert(result.runEither(0))(isLeft(equalTo("oh crap")))
              }
            ),
            suite("rightOrFailWith")(
              test("failure") {
                val result = ZPure.fail("fail").rightOrFailWith[Any, Any, String](_ => "Oh crap")
                assert(result.runEither(0))(isLeft(equalTo("fail")))
              },
              test("right") {
                val result = ZPure
                  .succeed[Int, Either[Nothing, Int]](Right(1))
                  .rightOrFailWith[Any, Int, String](_ => "oh crap")
                assert(result.runEither(0))(isRight(equalTo((0, 1))))
              },
              test("left") {
                val result = ZPure.succeed[Int, Either[String, Int]](Left("Left")).rightOrFail("oh crap")
                assert(result.runEither(0))(isLeft(equalTo("oh crap")))
              }
            ),
            suite("rightOrFailWithException")(
              test("failure") {
                val result = ZPure.fail(new NoSuchElementException()).rightOrFailWithException
                assert(result.runEither(0))(isLeft(isSubtype[NoSuchElementException](anything)))
              },
              test("right") {
                val result = ZPure.succeed[Int, Either[Nothing, Int]](Right(1)).rightOrFailWithException
                assert(result.runEither(0))(isRight(equalTo((0, 1))))
              },
              test("left") {
                val result = ZPure.succeed[Int, Either[String, Int]](Left("Left")).rightOrFailWithException
                assert(result.runEither(0))(isLeft(isSubtype[NoSuchElementException](anything)))
              }
            )
          ),
          suite("some")(
            testM("success (Some)") {
              check(genInt, genInt, genInt) { (s1, s2, a) =>
                val successSome: ZPure[Nothing, Int, Int, Any, Nothing, Option[Int]] = ZPure.modify(_ => (s2, Some(a)))
                val result: ZPure[Nothing, Int, Int, Any, Option[Nothing], Int]      = successSome.some
                assert(result.runEither(s1))(isRight(equalTo((s2, a))))
              }
            },
            testM("success (None)") {
              check(genInt) { s =>
                val successNone: ZPure[Nothing, Int, Int, Any, Nothing, Option[Int]] = ZPure.succeed(None)
                val result: ZPure[Nothing, Int, Int, Any, Option[Nothing], Int]      = successNone.some
                assert(result.runEither(s))(isLeft(isNone))
              }
            },
            testM("failure") {
              check(genInt, genInt) { (s, e) =>
                val failure: ZPure[Nothing, Int, Int, Any, Int, Option[Int]] = ZPure.fail(e)
                val result: ZPure[Nothing, Int, Int, Any, Option[Int], Int]  = failure.some
                assert(result.runEither(s))(isLeft(isSome(equalTo(e))))
              }
            }
          ),
          suite("someOrElse")(
            testM("success (Some)") {
              check(genInt, genInt, genInt, genInt) { (s1, s2, a, default) =>
                val successSome: ZPure[Nothing, Int, Int, Any, Nothing, Option[Int]] = ZPure.modify(_ => (s2, Some(a)))
                val result: ZPure[Nothing, Int, Int, Any, Nothing, Int]              = successSome.someOrElse(default)
                assert(result.run(s1))(equalTo((s2, a)))
              }
            },
            testM("success (None)") {
              check(genInt, genInt, genInt) { (s1, s2, default) =>
                val successNone: ZPure[Nothing, Int, Int, Any, Nothing, Option[Int]] = ZPure.modify(_ => (s2, None))
                val result: ZPure[Nothing, Int, Int, Any, Nothing, Int]              = successNone.someOrElse(default)
                assert(result.run(s1))(equalTo((s2, default)))
              }
            },
            testM("failure") {
              check(genInt, genInt, genInt) { (s, e, default) =>
                val failure: ZPure[Nothing, Int, Int, Any, Int, Option[Int]] = ZPure.fail(e)
                val result: ZPure[Nothing, Int, Int, Any, Int, Int]          = failure.someOrElse(default)
                assert(result.runEither(s))(isLeft(equalTo(e)))
              }
            }
          ),
          suite("someOrElseM")(
            testM("success (Some)") {
              check(genInt, genInt, genInt) { (s1, s2, a) =>
                val successSome: ZPure[Nothing, Int, Int, Any, Nothing, Option[Int]] = ZPure.modify(_ => (s2, Some(a)))
                val that: ZPure[Nothing, Int, Int, Any, Unit, Int]                   = ZPure.fail(())
                val result: ZPure[Nothing, Int, Int, Any, Unit, Int]                 = successSome.someOrElseM(that)
                assert(result.runEither(s1))(isRight(equalTo((s2, a))))
              }
            },
            testM("success (None)") {
              check(genInt, genInt, genIntToInt, genIntToInt) { (s, a, f1, f2) =>
                val successNone: ZPure[Nothing, Int, Int, Any, Nothing, Option[Int]] =
                  ZPure.modify(s1 => (f1(s1), None))
                val that: ZPure[Nothing, Int, Int, Any, Nothing, Int]                = ZPure.modify(s2 => (f2(s2), a))
                val result: ZPure[Nothing, Int, Int, Any, Nothing, Int]              = successNone.someOrElseM(that)
                assert(result.run(s))(equalTo((f2(f1(s)), a)))
              }
            },
            testM("failure") {
              check(genInt, genInt, genState) { (s, e, that) =>
                val failure: ZPure[Nothing, Int, Int, Any, Int, Option[Int]] = ZPure.fail(e)
                val result: ZPure[Nothing, Int, Int, Any, Int, Int]          = failure.someOrElseM(that)
                assert(result.runEither(s))(isLeft(equalTo(e)))
              }
            }
          ),
          suite("someOrFail")(
            testM("success (Some)") {
              check(genInt, genInt, genInt, genInt) { (s1, s2, e, a) =>
                val successSome: ZPure[Nothing, Int, Int, Any, Nothing, Option[Int]] = ZPure.modify(_ => (s2, Some(a)))
                val result: ZPure[Nothing, Int, Int, Any, Int, Int]                  = successSome.someOrFail(e)
                assert(result.runEither(s1))(isRight(equalTo((s2, a))))
              }
            },
            testM("success (None)") {
              check(genInt, genInt) { (s, e) =>
                val successNone: ZPure[Nothing, Int, Int, Any, Nothing, Option[Int]] = ZPure.succeed(None)
                val result: ZPure[Nothing, Int, Int, Any, Int, Int]                  = successNone.someOrFail(e)
                assert(result.runEither(s))(isLeft(equalTo(e)))
              }
            },
            testM("failure") {
              check(genInt, genInt, genInt) { (s, e1, e2) =>
                val failure: ZPure[Nothing, Int, Int, Any, Int, Option[Int]] = ZPure.fail(e1)
                val result: ZPure[Nothing, Int, Int, Any, Int, Int]          = failure.someOrFail(e2)
                assert(result.runEither(s))(isLeft(equalTo(e1)))
              }
            }
          ),
          suite("someOrFailException")(
            testM("success (Some)") {
              check(genInt, genInt, genInt) { (s1, s2, a) =>
                val successSome: ZPure[Nothing, Int, Int, Any, Nothing, Option[Int]]   = ZPure.modify(_ => (s2, Some(a)))
                val result: ZPure[Nothing, Int, Int, Any, NoSuchElementException, Int] = successSome.someOrFailException
                assert(result.runEither(s1))(isRight(equalTo((s2, a))))
              }
            },
            testM("success (None)") {
              check(genInt) { (s) =>
                val successNone: ZPure[Nothing, Int, Int, Any, Nothing, Option[Int]]   = ZPure.succeed(None)
                val result: ZPure[Nothing, Int, Int, Any, NoSuchElementException, Int] = successNone.someOrFailException
                assert(result.runEither(s))(isLeft(anything))
              }
            },
            testM("failure") {
              check(genInt, genInt) { (s, e) =>
                val failure: ZPure[Nothing, Int, Int, Any, Int, Option[Int]] = ZPure.fail(e)
                val result: ZPure[Nothing, Int, Int, Any, Any, Int]          = failure.someOrFailException
                assert(result.runEither(s))(isLeft(isSubtype[Int](equalTo(e))))
              }
            }
          ),
          suite("withFilter")(
            test("withFilter success") {
              val zpure1: ZPure[Any, Unit, Unit, Any, NoSuchElementException, (Int, Int)] = ZPure.succeed((1, 2))
              val zpure2: ZPure[Any, Unit, Unit, Any, RuntimeException, Int]              = ZPure.succeed(3)

              val program = for {
                (i, j)   <- zpure1
                positive <- zpure2 if positive > 0
              } yield positive
              assert(program.runEither(()))(isRight(equalTo(((), 3))))
            },
            test("withFilter fail") {
              val zpure1: ZPure[Any, Unit, Unit, Any, RuntimeException, (Int, Int)] = ZPure.succeed((1, 2))
              val zpure2: ZPure[Any, Unit, Unit, Any, NoSuchElementException, Int]  = ZPure.succeed(-3)

              val program = for {
                (i, j)   <- zpure1
                positive <- zpure2 if positive > 0
              } yield positive
              assert(program.runEither(())) {
                implicit val eq: Equal[RuntimeException] = Equal.ThrowableHash
                isLeft(
                  equalTo(
                    new NoSuchElementException(
                      "The value doesn't satisfy the predicate"
                    ): RuntimeException // upcast to RuntimeException because of Dotty
                  )
                )
              }
            }
          )
        ),
        suite("reject")(
          testM("success") {
            check(genInt, genInt, genInt) { (s1, a1, e1) =>
              val result = ZPure.succeed[Int, Int](a1).reject { case _ =>
                e1
              }
              assert(result.runEither(s1))(isLeft(equalTo(e1)))
            }
          },
          testM("failure") {
            check(genInt, genInt, genInt) { (s1, a1, e1) =>
              val result = ZPure.succeed[Int, Int](a1).reject {
                case _ if false => e1
              }
              assert(result.runEither(s1))(isRight(equalTo((s1, a1))))
            }
          }
        ),
        suite("rejectM")(
          testM("success") {
            check(genInt, genInt, genInt) { (s1, a1, e1) =>
              val result = ZPure.succeed[Int, Int](a1).rejectM { case _ =>
                ZPure.succeed[Int, Int](e1)
              }
              assert(result.runEither(s1))(isLeft(equalTo(e1)))
            }
          },
          testM("failure") {
            check(genInt, genInt, genInt) { (s1, a1, e1) =>
              val result = ZPure.succeed[Int, Int](a1).rejectM {
                case _ if false => ZPure.succeed[Int, Int](e1)
              }
              assert(result.runEither(s1))(isRight(equalTo((s1, a1))))
            }
          }
        ),
        suite("constructors")(
          testM("fail") {
            check(genInt) { e =>
              assert(ZPure.fail(e).runEither(()))(isLeft(equalTo(e)))
            }
          },
          testM("fromEither (Left)") {
            check(genString) { l =>
              assert(ZPure.fromEither(Left(l)).runEither(()))(isLeft(equalTo(l)))
            }
          },
          testM("fromEither (Right)") {
            check(genString) { r =>
              val (_, a) = ZPure.fromEither(Right(r)).run(())
              assert(a)(equalTo(r))
            }
          },
          test("fromOption (None)") {
            assert(ZPure.fromOption(Option.empty[String]).runEither(()))(isLeft(equalTo(())))
          },
          testM("fromOption (Some)") {
            check(genInt) { a =>
              assert(ZPure.fromOption(Option(a)).runEither(()))(isRight(equalTo(((), a))))
            }
          },
          testM("fromTry (Success case)") {
            check(genInt) { a =>
              assert(ZPure.fromTry(Try(a)).runEither(()))(isRight(equalTo(((), a))))
            }
          },
          test("fromTry (Failure case)") {
            implicit val throwableHash = Equal.ThrowableHash
            val exception: Throwable   = new NumberFormatException("""For input string: "a"""")
            assert(ZPure.fromTry(Try("a".toInt)).runEither(()))(
              isLeft(equalTo(exception))
            )
          },
          testM("fromEffect (Success case)") {
            check(genInt) { a =>
              assert(ZPure.fromEffect(a).runEither(()))(isRight(equalTo(((), a))))
            }
          },
          test("fromEffect (Failure case)") {
            implicit val throwableHash = Equal.ThrowableHash
            val exception: Throwable   = new NumberFormatException("""For input string: "a"""")
            assert(ZPure.fromEffect("a".toInt).runEither(()))(
              isLeft(equalTo(exception))
            )
          },
          suite("modifyEither")(
            test("success") {
              assert(ZPure.modifyEither((_: Int) => Right((1, "success"))).run(0))(equalTo((1, "success")))
            },
            test("failure") {
              assert(ZPure.modifyEither((_: Int) => Left("error")).runEither(0))(isLeft(equalTo("error")))
            }
          )
        )
      ),
      suite("error")(
        test("parallel errors example") {
          def validateName(s: String): Validation[String, String]               =
            if (s == "John Doe") Validation.succeed(s) else Validation.fail("Wrong name!")
          def validateAge(age: Int): Validation[String, Int]                    =
            if (age >= 18) Validation.succeed(age) else Validation.fail("Under age")
          def validateAuthorized(authorized: Boolean): Validation[String, Unit] =
            if (authorized) Validation.unit else Validation.fail("Not authorized")
          val validation                                                        = validateName("Jane Doe") zipPar0
            validateAge(17) zipPar0
            validateAuthorized(false)
          val result                                                            = validation.runEitherCause(())
          assert(result)(
            isLeft(equalTo(Cause("Wrong name!") && Cause("Under age") && Cause("Not authorized")))
          )
        }
      ),
      suite("log")(
        test("logging example") {
          val zPure = ZPure.unit[Unit].log("first").log("second").log("third").getLog
          assert(zPure.runResult(()))(equalTo(Chunk("first", "second", "third")))
        }
      )
    )
}
