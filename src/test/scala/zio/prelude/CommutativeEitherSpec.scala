package zio.prelude

import zio.ZIO
import zio.test._

import scala.concurrent.{ Future, blocking }

object CommutativeEitherSpec extends DefaultRunnableSpec {

  @SuppressWarnings(Array("scalafix:DisableSyntax.noSemicolons"))
  def spec: Spec[Any, TestFailure[Throwable], TestSuccess] =
    suite("CommutativeEitherSpec")(
      testM("FutureCommutativeEither returns the first future that is completed") {
        for {
          l <- ZIO.fromFuture { implicit ec =>
                 Future.successful("immediate") <|> Future(blocking { Thread.sleep(60 * 1000); "long 1" })
               }
          r <- ZIO.fromFuture { implicit ec =>
                 Future(blocking { Thread.sleep(60 * 1000); "long 2" }) <|> Future.successful("immediate")
               }
        } yield assert(l.merge)(equalTo(r.merge))
      }
    )
}
