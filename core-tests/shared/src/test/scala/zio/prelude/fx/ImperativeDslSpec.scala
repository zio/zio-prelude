package zio.prelude.fx

import zio.prelude._
import zio.test._
import ImperativeDslSpec.transitSystem.{ops, dsl}
import ops.Card
import scala.language.reflectiveCalls

object ImperativeDslSpec extends ZIOBaseSpec {
  def spec: Spec[Environment, Any] = suite("ImperativeDslSpec")(
    suite("unsafeInterpret")(
      test("Interpreting a getRiderCount after 2 authorized riders") {
        val john = new {
          val ridesCard = Card.TransitRideCard(2)
        }

        val jane = new {
          val debitCard = Card.DebitCard(10_00)
        }

        val interpreter = transitSystem.interpreters.default(farePriceInCents = 2_50)

        val program = for {
          _ <- dsl.authorize(john.ridesCard)
          _ <- dsl.authorize(jane.debitCard)
          c <- dsl.getRiderCount
        } yield c

        val result = program.interpret(interpreter)
        val actual = result.runEither

        assertTrue(actual == Right(2))
      }
    )
  )

  object transitSystem {
    object ops {
      sealed trait TransitSystemOp[+E, +A]   extends Product with Serializable
      final case class Authorize(card: Card) extends TransitSystemOp[AccessDeniedError, Card]
      case object GetRiderCount              extends TransitSystemOp[Nothing, Int]

      sealed trait Card
      object Card {
        final case class DebitCard(balance: Int)     extends Card
        final case class TransitRideCard(rides: Int) extends Card
      }

      sealed trait TransitSystemError
      sealed trait AccessDeniedError extends TransitSystemError
      object TransitSystemError {
        final case object InsufficientBalance extends AccessDeniedError
        final case object NoRides             extends AccessDeniedError
      }
    }

    object dsl {
      import ops._

      type TSys[+E, +A] = ImperativeDsl[TransitSystemOp, E, A]

      def authorize(card: Card): TSys[AccessDeniedError, Card] =
        ImperativeDsl.eval(Authorize(card))

      def getRiderCount: TSys[Nothing, Int] = ImperativeDsl.eval(GetRiderCount)
    }

    object interpreters {
      import ImperativeDsl.Interpreter
      import ops._
      type Result[+E, +A] = zio.prelude.fx.ZPure[String, Unit, Unit, Any, E, A]
      def default(farePriceInCents: Int, initialRiderCount: Int = 0): Interpreter[TransitSystemOp, Result] = {
        var riderCount = initialRiderCount
        new Interpreter[TransitSystemOp, Result] {
          override def interpret[E, A](fa: TransitSystemOp[E, A]): Result[E, A] =
            fa match {
              case Authorize(card) =>
                card match {
                  case Card.DebitCard(balance)     =>
                    if (balance >= farePriceInCents) {
                      riderCount += 1
                      ZPure.succeed(Card.DebitCard(balance - farePriceInCents))
                    } else {
                      ZPure.fail(TransitSystemError.InsufficientBalance)
                    }
                  case Card.TransitRideCard(rides) =>
                    if (rides > 0) {
                      riderCount += 1
                      ZPure.succeed(Card.TransitRideCard(rides - 1))
                    } else {
                      ZPure.fail(TransitSystemError.NoRides)
                    }
                }
              case GetRiderCount   => ZPure.succeed(riderCount)
            }
        }
      }
    }
  }
}
