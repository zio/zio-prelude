package zio.prelude.fx

import zio.prelude._
import zio.random.Random
import zio.test.Assertion.isLeft
import zio.test._

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
          }
        )
      ),
      suite("state")(
        suite("methods")(
          testM("contramap") {
            check(genState, genIntToInt, genInt) { (fa, f, s) =>
              val (s1, a1) = fa.run(s)
              assert(fa.mapState(f).run(s))(equalTo((f(s1), a1)))
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
            check(genString, genInt, genInt) { (s1, v, v1) =>
              val succeeded = ZPure.succeed[Seq[String], Int](v).orElseSucceed(v1)
              val (_, a)    = succeeded.run(List(s1))
              assert(a)(equalTo(v))
            }
          },
          testM("orElseSucceed (Failure case)") {
            check(genString, genString, genInt) { (s1, e, v1) =>
              val failed = ZPure.fail(e).orElseSucceed(v1)
              val (_, a) = failed.run(s1)
              assert(a)(equalTo(v1))
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
              check(genInt, genInt, genIntToInt, genIntToInt) { (s1, a1, failure, success) =>
                val (s2, a2) = ZPure.succeed[Int, Int](a1).fold(failure, success).run(s1)
                assert(s2)(equalTo(s1)) && assert(a2)(equalTo(success(a1)))
              }
            }
          ),
          suite("foldM")(
            test("failure") {
              val failing =
                ZPure.succeed[Int, Int](1).flatMap(n => if (n % 2 !== 0) ZPure.fail("fail") else ZPure.succeed(n))
              val result  = failing.foldM(
                _ => State.update[Int, Int](_ + 1) *> ZPure.succeed(0),
                a => State.update[Int, Int](_ + 2) *> ZPure.succeed(a)
              )
              assert(result.run(10))(equalTo((11, 0)))
            },
            test("success") {
              val failing =
                ZPure.succeed[Int, Int](2).flatMap(n => if (n % 2 !== 0) ZPure.fail("fail") else ZPure.succeed(n))
              val result  = failing.foldM(
                _ => State.update[Int, Int](_ + 1) *> ZPure.succeed(0),
                a => State.update[Int, Int](_ + 2) *> ZPure.succeed(a)
              )
              assert(result.run(10))(equalTo((12, 2)))
            }
          )
        ),
        suite("constructors")(
          testM("fail") {
            check(genInt) { e =>
              assert(ZPure.fail(e).runEither(()))(isLeft(equalTo(e)))
            }
          }
        )
      )
    )
}
