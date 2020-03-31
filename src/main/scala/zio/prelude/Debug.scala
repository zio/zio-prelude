package zio.prelude

trait Debug[-A] {
  def debug(a: A): Debug.Repr
}

object Debug {
  def apply[A](implicit debug: Debug[A]): Debug[A] = debug

  def make[A](f: A => Debug.Repr): Debug[A] = f(_)

  sealed trait Repr { self =>
    override def toString: String = self match {
      case Repr.Int(v)        => v.toString
      case Repr.Double(v)     => v.toString
      case Repr.Float(v)      => v.toString
      case Repr.Long(v)       => v.toString
      case Repr.Byte(v)       => v.toString
      case Repr.Char(v)       => v.toString
      case Repr.String(v)     => v.toString
      case Repr.Object(ns, n) => s"${ns.mkString(".")}.$n"
      case Repr.Constructor(ns, n, reprs) =>
        s"${ns.mkString(".")}.$n(${reprs.map(kv => s"${kv._1} -> ${kv._2}").mkString(", ")})"
      case Repr.VConstructor(ns, n, reprs) => s"${ns.mkString(".")}.$n(${reprs.mkString(", ")})"
    }
  }

  object Repr {
    import scala.{ Int => SInt, Double => SDouble, Float => SFloat, Long => SLong, Char => SChar, Byte => SByte }
    import java.lang.{ String => SString }

    final case class Int(value: SInt)       extends Repr
    final case class Double(value: SDouble) extends Repr
    final case class Float(value: SFloat)   extends Repr
    final case class Long(value: SLong)     extends Repr
    final case class Byte(value: SByte)     extends Repr
    final case class Char(value: SChar)     extends Repr
    final case class String(value: SString) extends Repr

    final case class Object(namespace: List[SString], name: SString)                                 extends Repr
    final case class Constructor(namespace: List[SString], name: SString, reprs: Map[SString, Repr]) extends Repr
    object Constructor {
      def apply(namespace: List[SString], name: SString, repr: (SString, Repr), reprs: (SString, Repr)*): Repr =
        new Constructor(namespace, name, (repr :: reprs.toList).toMap)
    }
    final case class VConstructor(namespace: List[SString], name: SString, reprs: List[Repr]) extends Repr
  }

  implicit val NothingDebug: Debug[Nothing] = n => n
  implicit val UnitDebug: Debug[Unit]       = _ => Repr.Object("scala" :: Nil, "()")
  implicit val IntDebug: Debug[Int]         = Repr.Int(_)
  implicit val DoubleDebug: Debug[Double]   = Repr.Double(_)
  implicit val FloatDebug: Debug[Float]     = Repr.Float(_)
  implicit val LongDebug: Debug[Long]       = Repr.Long(_)
  implicit val ByteDebug: Debug[Byte]       = Repr.Byte(_)
  implicit val CharDebug: Debug[Char]       = Repr.Char(_)
  implicit val StringDebug: Debug[String]   = Repr.String(_)

  implicit def EitherDebug[E: Debug, A: Debug]: Debug[Either[E, A]] =
    either =>
      either match {
        case Left(e)  => Repr.VConstructor(List("scala"), "Left", List(e.debug))
        case Right(a) => Repr.VConstructor(List("scala"), "Right", List(a.debug))
      }

  implicit def OptionDebug[A: Debug]: Debug[Option[A]] =
    option =>
      option match {
        case None    => Repr.Object(List("scala"), "None")
        case Some(a) => Repr.VConstructor(List("scala"), "Some", List(a.debug))
      }

  implicit def ListDebug[A: Debug]: Debug[List[A]] =
    list => Repr.VConstructor(List("scala"), "List", list.map(_.debug))

  implicit def VectorDebug[A: Debug]: Debug[Vector[A]] =
    vector => Repr.VConstructor(List("scala"), "Vector", vector.map(_.debug).toList)

  implicit def MapDebug[K: Debug, V: Debug]: Debug[Map[K, V]] =
    map => Repr.VConstructor(List("scala"), "Map", List(map.toList.debug))

  implicit def Tuple2Debug[A: Debug, B: Debug]: Debug[(A, B)] =
    tup2 => Repr.VConstructor(List("scala"), "Tuple2", List(tup2._1.debug, tup2._2.debug))
}

trait DebugSyntax {
  implicit class DebugOps[A](self: A) {
    def debug(implicit debug: Debug[A]): Debug.Repr = debug.debug(self)
  }
}
