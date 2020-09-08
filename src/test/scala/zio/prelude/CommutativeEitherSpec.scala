package zio.prelude

import scala.concurrent.{ Future, blocking }

import zio.ZIO
import zio.test._

object CommutativeEitherSpec extends DefaultRunnableSpec {

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
