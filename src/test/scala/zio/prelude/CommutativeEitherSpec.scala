package zio.prelude

import scala.concurrent.Future

import zio.ZIO
import zio.test._

object CommutativeEitherSpec extends DefaultRunnableSpec {

  def spec = suite("CommutativeEitherSpec")(
    testM("FutureCommutativeEither returns the first future that is completed") {
      for {
        l <- ZIO.fromFuture { implicit ec =>
              Future.unit <|> Future.never
            }
        r <- ZIO.fromFuture { implicit ec =>
              Future.never <|> Future.unit
            }
      } yield assert(l.swap)(equalTo(r))
    }
  )
}
