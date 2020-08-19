package zio.prelude

import scala.concurrent.{ blocking, Future }
import zio.ZIO
import zio.test._

object CommutativeEitherSpec extends DefaultRunnableSpec {

  def spec = suite("CommutativeEitherSpec")(
    testM("FutureCommutativeEither returns the first future that is completed") {
      for {
        l <- ZIO.fromFuture { implicit ec =>
              Future.successful(()) <|> Future {
                blocking {
                  while (true) {
                    Thread.sleep(5000)
                    println("infinite future 1")
                  }
                }
              }
            }
        r <- ZIO.fromFuture { implicit ec =>
              Future {
                blocking {
                  while (true) {
                    Thread.sleep(5000)
                    println("infinite future 2")
                  }
                }
              } <|> Future.successful(())
            }
      } yield assert(l.merge)(equalTo(r.merge))
    }
  )
}
