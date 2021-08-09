package zio.prelude

import zio.prelude.coherent.ReadEqual
import zio.test.TestResult
import zio.test.laws.{Lawful, Laws}

trait Read[A] extends Debug[A] {
  def fromDebug(debug: Debug.Repr): Either[Read.ReadException.ReprMismatch, A]
  final def read(string: String): Either[Read.ReadException, A] = Read.parse(string).flatMap(fromDebug)
}

object Read extends Lawful[ReadEqual] {

  /**
   * The associativity law states all values that are serialized to a string can be successfully read back:
   *
   * {{{
   * a.debug.render.read === Right(a)
   * }}}
   */
  lazy val readLaw: Laws[ReadEqual] =
    new Laws.Law1[ReadEqual]("readLaw") {
      def apply[A: ReadEqual](a: A): TestResult = {
        implicit val throwableHash: Equal[ReadException] = Equal.ThrowableHash
        a.debug.render.read <-> Right(a)
      }
    }

  /**
   * The set of all laws that instances of `Read` must satisfy.
   */
  lazy val laws: Laws[ReadEqual] =
    readLaw

  sealed abstract class ReadException(message: String) extends RuntimeException(message)

  object ReadException {
    final case class ReprMismatch(expected: String, got: Debug.Repr)
        extends ReadException(s"Expected a $expected, but got '$got'.")

    final case class ParseError(string: String) extends ReadException(s"Can't parse '$string'.")
  }

  private object Pattern {
    val String = "\"(.*)\"".r
    val Char   = "'(.)'".r
  }

  def parse(string: String): Either[ReadException.ParseError, Debug.Repr] = string match {
    case Pattern.String(value) => Right(Debug.Repr.String(StringContext.processEscapes(value)))
    case Pattern.Char(value)   => Right(Debug.Repr.Char(value.charAt(0)))
    case _                     =>
      val boolean = string.toBooleanOption
      if (boolean.isDefined) {
        return Right(Debug.Repr.Boolean(boolean.get))
      }
      val int     = string.toIntOption
      if (int.isDefined) {
        return Right(Debug.Repr.Int(int.get))
      }
      if (string.lastOption.contains('L')) {
        val long = string.init.toLongOption
        if (long.isDefined) {
          return Right(Debug.Repr.Long(long.get))
        }
      }
      if (string.lastOption.contains('f')) {
        val float = string.toFloatOption
        if (float.isDefined) {
          return Right(Debug.Repr.Float(float.get))
        }
      }
      val double  = string.toDoubleOption
      if (double.isDefined) {
        return Right(Debug.Repr.Double(double.get))
      }
      Left(ReadException.ParseError(string))
  }

}

trait ReadSyntax {

  implicit class ReadStringOps(self: String) {
    def toDebug: Either[Read.ReadException.ParseError, Debug.Repr]  = Read.parse(self)
    def read[A](implicit A: Read[A]): Either[Read.ReadException, A] = A.read(self)
  }

  implicit class ReadDebugOps(self: Debug.Repr) {
    def read[A](implicit A: Read[A]): Either[Read.ReadException, A] = A.fromDebug(self)
  }

}
