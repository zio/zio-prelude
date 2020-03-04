package zio.prelude

import zio.App
import zio.console._

object HelloWorld extends App {
  trait Json

  /**
   * For all a: A:
   *   fromJson(toJson(a)) == Some(a)
   */
  trait JsonCodec[A] {
    def toJson(self: A): Json

    def fromJson(json: Json): Option[A]

    final def identityLaw(a: A): Boolean =
      fromJson(toJson(a)) == Some(a)
  }
  object JsonCodec {
    def apply[A](implicit js: JsonCodec[A]): JsonCodec[A] = js

    implicit def ListJsonCodec[A: JsonCodec]: JsonCodec[List[A]] = ???
  }
  implicit class JsonCodecSyntax[A](val a: A) extends AnyVal {
    def toJson(implicit js: JsonCodec[A]): Json = js.toJson(a)
  }

  final case class Person(name: String, age: Int)
  object Person {
    implicit val PersonJsonCodec: JsonCodec[Person] =
      new JsonCodec[Person] {
        def toJson(self: Person): Json = ???

        def fromJson(json: Json): Option[Person] = ???
      }
  }

  final case class MyPerson(person: Person)
  object MyPerson {
    //
  }

  // check(genPerson) { PersonJsonCodec.identityLaw(_) }

  def dumpToFile[A: JsonCodec](a: A): Unit          = ???
  def readFromFile[A: JsonCodec]: scala.util.Try[A] = ???

  lazy val person: Person           = ???
  lazy val personList: List[Person] = ???

  JsonCodec[Person].toJson(person)

  dumpToFile(person)
  dumpToFile(personList)

  // java.util.ArrayList

  def run(args: List[String]) =
    myAppLogic.fold(_ => 1, _ => 0)

  val myAppLogic =
    for {
      _    <- putStrLn("Hello! What is your name?")
      name <- getStrLn
      _    <- putStrLn(s"Hello, $name, welcome to ZIO!")
    } yield ()
}
