package zio.prelude

import zio.random.Random
import zio.test._

object ZStateSpec extends DefaultRunnableSpec {

  lazy val genInt: Gen[Random, Int] =
    Gen.anyInt

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

  def spec =
    suite("ZStateSpec")(
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
            assert(State(f).run(s))(equalTo(f(s)))
          }
        },
        testM("runResult") {
          check(genIntToIntInt, genInt) { (f, s) =>
            assert(State(f).runResult(s))(equalTo(f(s)._2))
          }
        },
        testM("runState") {
          check(genIntToIntInt, genInt) { (f, s) =>
            assert(State(f).runState(s))(equalTo(f(s)._1))
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
    )
}
