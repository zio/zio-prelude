package zio.prelude

import zio.random.Random
import zio.test._
import zio.test.laws._

object ValidationSpec extends DefaultRunnableSpec {

  val genValidation: Gen[Random with Sized, Validation[Int, Int]] =
    Gens.validation(Gen.anyInt, Gen.anyInt)

  def spec = suite("ValidationSpec")(
    suite("laws")(
      testM("equal")(checkAllLaws(Equal)(genValidation)),
      testM("hash")(checkAllLaws(Hash)(genValidation)),
      testM("ord")(checkAllLaws(Ord)(genValidation))
    )
  )
}
