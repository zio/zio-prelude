package zio.prelude.newtypes

import zio.prelude.newtypes.derivation.derive
import zio.prelude.{Newtype, Subtype}
import zio.test._

object DerivationSpec extends DefaultRunnableSpec {
  trait Show[A] {
    def show(a: A): String
  }

  implicit val showString: Show[String] = (a: String) => a

  // these are imported from an external library - we have no control over them
  object External {
    object Secret extends Newtype[String]
    type Secret = Secret.Type

    object Password extends Subtype[String]
    type Password = Password.Type
  }

  import External._

  override def spec: ZSpec[Environment, Failure] = suite("external derivation for newtypes")(
    test("works for Newtype") {
      val showSecret: Show[Secret] = derive
      assertTrue(showSecret.show(Secret("foo")) == "foo")
    },
    test("works for Subtype") {
      val showPassword: Show[Password] = derive
      assertTrue(showPassword.show(Password("foo")) == "foo")
    },
    testM("works with auto derivation") {
      typeCheck("""
                  |import zio.prelude.newtypes.derivation.auto._
                  |implicitly[Show[Secret]]
                  |implicitly[Show[Password]]
                  |""".stripMargin).absolve
        .as(assertCompletes)
    }
  )
}
