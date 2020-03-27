package zio.prelude

trait Debug[-A]{
  def debug(a: A): Debug.Repr
}
object Debug {
  def apply[A](implicit debug: Debug[A]): Debug[A] = debug

  def make[A](f: A => Debug.Repr): Debug[A] = f(_)

  // primitives, case classes, sealed traits, collections
  sealed trait Repr {
    override def toString: String = ???
  }
  object Repr {
    import scala.{ Int => SInt, Double => SDouble }
    import java.lang.{ String => SString }

    final case class Int(value: SInt)       extends Repr
    final case class Double(value: SDouble) extends Repr
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
  implicit val StringDebug: Debug[String]   = Repr.String(_)
  implicit def ListDebug[A: Debug]: Debug[List[A]] =
    list => Repr.VConstructor(List("scala"), "List", list.map(_.debug))
  implicit def VectorDebug[A: Debug]: Debug[Vector[A]] =
    vector => Repr.VConstructor(List("scala"), "Vector", vector.map(_.debug).toList)
}
trait DebugSyntax {
  implicit class DebugSyntax[A](self: A) {
    def debug(implicit debug: Debug[A]): Debug.Repr = debug.debug(self)
  }
}

/*

  case class Person(name: String, age: Int, title: Title, addresses: List[String])
  object Person {
    implicit val PersonDebug: Debug[Person] = person =>
      Repr.Constructor(
        "zio" :: "prelude" :: Nil,
        "Debug.Person",
        "name"      -> person.name.debug,
        "age"       -> person.age.debug,
        "title"     -> person.title.debug,
        "addresses" -> person.addresses.debug)
  }

  sealed trait Title
  object Title {
    case object Engineer extends Title
    case object Architect extends Title
    case object Devops extends Title

    implicit val TitleDebug: Debug[Title] = {
      case Engineer => Repr.Object(List("zio", "prelude"), "Debug.Title.Engineer")
      case Architect => Repr.Object(List("zio", "prelude"), "Debug.Title.Architect")
      case Devops => Repr.Object(List("zio", "prelude"), "Debug.Title.Devops")
    }
  }
 */
