package zio.prelude

import zio.ZIO
import zio.prelude.laws._
import zio.test._

import scala.concurrent.{Future, blocking}

object FutureCommutativeEitherSpec extends DefaultRunnableSpec {

  def spec: Spec[Any, TestFailure[Throwable], TestSuccess] =
    suite("FutureCommutativeEitherSpec")(
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
