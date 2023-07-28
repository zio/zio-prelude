package zio.prelude.fx

import zio.prelude._
import zio.test._
import ImperativeSpec.transitSystem.{Dsl, syntax}
import Dsl.Card

object ImperativeSpec extends ZIOBaseSpec {
  def spec: Spec[Environment, Any] = suite("ImperativeSpec")(
    suite("unsafeInterpret")(
      test("Interpreting a getRiderCount after 2 authorized riders") {
        import syntax._
        case class Customer(name:String, card:Card)

        val john = Customer("John", Card.TransitRideCard(2))
        val jane = Customer("Jane", Card.DebitCard(1000))

        val interpreter = transitSystem.interpreters.default(farePriceInCents = 250)

        val program = for {
          _ <- authorize(john.card)
          _ <- authorize(jane.card)
          cnt <- getRiderCount
        } yield cnt

        val result = program.interpret(interpreter)
        val actual = result.runEither

        assertTrue(actual == Right(2))
      }
    )
  )

  object transitSystem {
    object Dsl {
      sealed trait TransitSystemDsl[+E, +A]   extends Product with Serializable
      final case class Authorize(card: Card) extends TransitSystemDsl[AccessDeniedError, Card]
      case object GetRiderCount              extends TransitSystemDsl[Nothing, Int]

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

    object syntax {
      import Dsl._

      type TSys[+E, +A] = Imperative[TransitSystemDsl, E, A]

      def authorize(card: Card): TSys[AccessDeniedError, Card] =
        Imperative.eval(Authorize(card))

      def getRiderCount: TSys[Nothing, Int] = Imperative.eval(GetRiderCount)
    }

    object interpreters {
      import Imperative.Interpreter
      import Dsl._
      type Result[+E, +A] = zio.prelude.fx.ZPure[String, Unit, Unit, Any, E, A]
      def default(farePriceInCents: Int, initialRiderCount: Int = 0): Interpreter[TransitSystemDsl, Result] = {
        var riderCount = initialRiderCount
        new Interpreter[TransitSystemDsl, Result] {
          override def interpret[E, A](dsl: TransitSystemDsl[E, A]): Result[E, A] =
            dsl match {
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
