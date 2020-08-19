package zio.prelude

import scala.concurrent.{ blocking, Future }
import zio.ZIO
import zio.test._

object CommutativeEitherSpec extends DefaultRunnableSpec {

  def spec = suite("CommutativeEitherSpec")(
    testM("FutureCommutativeEither returns the first future that is completed") {
      for {
        l <- ZIO.fromFuture { implicit ec =>
              Future.successful(()) <|> Future { blocking(Thread.sleep(60 * 1000)) }
            }
        r <- ZIO.fromFuture { implicit ec =>
              Future { blocking(Thread.sleep(60 * 1000)) } <|> Future.successful(())
            }
      } yield assert(l.merge)(equalTo(r.merge))
    }
  )
}
