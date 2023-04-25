/*
 * Copyright 2020-2023 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.prelude

import zio.{Chunk, Duration => ZIODuration, NonEmptyChunk}

import java.util.concurrent.TimeUnit
import scala.collection.immutable.ListMap
import scala.concurrent.duration.{Duration => ScalaDuration}
import scala.language.implicitConversions

/**
 * `Debug` is an abstraction that describes the ability to render a value of
 * type `A` to a human readable format for debugging purposes.
 *
 * `Debug` captures this information in a structured data format called a
 * `Repr`, or a "representation" of the data. This representation can then be
 * rendered to a human readable format using a `Renderer`, which knows how to
 * render a representation to a specific human readable format. This two step
 * process preserves information when creating the representation and allows
 * rendering it in different ways. For example, we might want to render
 * it as a simple string representation using the `Simple` renderer or as valid
 * Scala code that we could paste into a REPL with the `Scala` renderer.
 *
 * You can use `Repr` to create structured representations of your own data
 * types and even implement your own `Renderer`, for example to render
 * representations to JSON, though in most cases the built in renderers will
 * be all that you need.
 */
trait Debug[-A] {

  /**
   * Returns a structured representation of the specified value that can be
   * rendered to a human readable format.
   */
  def debug(a: A): Debug.Repr

  /**
   * Render the specified value to a `String` using the `Simple` renderer.
   */
  final def render(a: A): String =
    debug(a).render
}

object Debug extends DebugVersionSpecific {

  /**
   * A `Renderer` knows how to convert a `Repr` to a human readable format.
   */
  type Renderer = Repr => String

  object Renderer {

    /**
     * A renderer that renders the `Repr` as a string containing the full
     * information in the `Repr`.
     */
    val Full: Renderer = {
      case Repr.KeyValue(k, v)             => s"key: ${k.render(Full)} -> value: ${v.render(Full)}"
      case Repr.Object(ns, n)              => (ns :+ n).mkString(".")
      case Repr.Constructor(ns, n, reprs)  =>
        (ns :+ s"$n(${reprs.map(kv => s"${kv._1} = ${kv._2.render(Full)}").mkString(", ")})").mkString(".")
      case Repr.VConstructor(ns, n, reprs) =>
        (ns :+ n).mkString(".") + s"(${reprs.map(_.render(Full)).mkString(", ")})"
      case any                             => Simple(any)
    }

    /**
     * A `Renderer` that renders the `Repr` as valid Scala code that could be
     * copy and pasted into an IDE or REPL.
     */
    val Scala: Renderer = {
      case Repr.Float(v)                                                               => v.toString
      case Repr.Long(v)                                                                => v.toString
      case Repr.Char(v)                                                                => v.toString
      case Repr.String(v)                                                              => v
      case Repr.KeyValue(k, v)                                                         => s"${k.render(Scala)} -> ${v.render(Scala)}"
      case Repr.Object(_, n)                                                           => n
      case Repr.Constructor(_, n, reprs)                                               =>
        s"$n(${reprs.map(kv => kv._2.render(Scala)).mkString(",")})"
      case Repr.VConstructor(_, n, reprs) if List("List", "Vector", "Map").contains(n) =>
        s"$n(${reprs.map(_.render(Scala)).mkString(", ")})"
      case Repr.VConstructor(List("scala"), n, reprs) if n.matches("^Tuple\\d+$")      =>
        s"(${reprs.map(_.render(Scala)).mkString(",")})"
      case Repr.VConstructor(_, n, reprs)                                              => s"$n(${reprs.map(_.render(Scala)).mkString(",")})"
      case any                                                                         => Simple(any)
    }

    /**
     * A renderer that renders the `Repr` as a simple string.
     */
    val Simple: Renderer = {
      case Repr.Int(v)                                                            => v.toString
      case Repr.Double(v)                                                         => v.toString
      case Repr.Float(v)                                                          => s"${v}f"
      case Repr.Long(v)                                                           => s"${v}L"
      case Repr.Byte(v)                                                           => v.toString
      case Repr.Char(v)                                                           => s"'$v'"
      case Repr.Boolean(v)                                                        => v.toString
      case Repr.Short(v)                                                          => v.toString
      case Repr.String(v)                                                         => s""""$v""""
      case Repr.KeyValue(k, v)                                                    => s"${k.render(Simple)} -> ${v.render(Simple)}"
      case Repr.Object(_, n)                                                      => n
      case Repr.Constructor(_, n, reprs)                                          =>
        s"$n(${reprs.map(kv => s"${kv._1} = ${kv._2.render(Simple)}").mkString(", ")})"
      case Repr.VConstructor(List("scala"), n, reprs) if n.matches("^Tuple\\d+$") =>
        s"(${reprs.map(_.render(Simple)).mkString(", ")})"
      case Repr.VConstructor(_, n, reprs)                                         => s"$n(${reprs.map(_.render(Simple)).mkString(", ")})"
    }
  }

  /**
   * A `Repr` is a structured representation of a value of that can be rendered
   * into various human readable formats.
   */
  sealed trait Repr { self =>

    /**
     * Render the `Repr` to a human readable format using the specified
     * `Renderer`.
     */
    def render(renderer: Renderer): String =
      renderer(self)

    /**
     * Render the `Repr` to a human readable format using the `Simple`
     * renderer.
     */
    def render: String =
      render(Renderer.Simple)
  }

  object Repr {
    import java.lang.{String => SString}
    import scala.{
      Boolean => SBoolean,
      Byte => SByte,
      Char => SChar,
      Double => SDouble,
      Float => SFloat,
      Int => SInt,
      Long => SLong,
      Short => SShort
    }

    /**
     * A structured representation of a `Boolean` value.
     */
    final case class Boolean(value: SBoolean) extends Repr

    /**
     * A structured representation of a `Byte` value.
     */
    final case class Byte(value: SByte) extends Repr

    /**
     * A structured representation of a `Char` value.
     */
    final case class Char(value: SChar) extends Repr

    /**
     * A structured representation of a class constructor including the names
     * of each field and their values.
     */
    final case class Constructor(namespace: List[SString], name: SString, reprs: ListMap[SString, Repr]) extends Repr

    object Constructor {

      /**
       * Constructs a structured representation of a class constructor with the
       * specified namespace, name, and fields.
       */
      def apply(namespace: List[SString], name: SString, repr: (SString, Repr), reprs: (SString, Repr)*): Repr =
        new Constructor(namespace, name, ListMap(repr :: reprs.toList: _*))
    }

    /**
     * A structured representation of a `Double` value.
     */
    final case class Double(value: SDouble) extends Repr

    /**
     * A structured representation of a `Float` value.
     */
    final case class Float(value: SFloat) extends Repr

    /**
     * A structured representation of an `Int` value.
     */
    final case class Int(value: SInt) extends Repr

    /**
     * A structured representation of a key value pair, as might exist in a
     * `Map`.
     */
    final case class KeyValue(key: Repr, value: Repr) extends Repr

    /**
     * A structured representation of a `Long` value.
     */
    final case class Long(value: SLong) extends Repr

    /**
     * A structured representation of a static object.
     */
    final case class Object(namespace: List[SString], name: SString) extends Repr

    /**
     * A structured representation of a `Short` value.
     */
    final case class Short(value: SShort) extends Repr

    /**
     * A structured representation of a `String` value.
     */
    final case class String(value: SString) extends Repr

    /**
     * A structured representation of a class constructor including the values
     * of each field.
     */
    final case class VConstructor(namespace: List[SString], name: SString, reprs: List[Repr]) extends Repr

    /**
     * Provides an implicit conversion from a value of a data type to a
     * structured representation of that value given a `Debug` instance. Used
     * to support the string interpolation functionality.
     */
    implicit def deriveRepr[A](a: A)(implicit debug: Debug[A]): Repr =
      debug.debug(a)
  }

  /**
   * Summons an implicit `Debug[A]`.
   */
  def apply[A](implicit debug: Debug[A]): Debug[A] =
    debug

  /**
   * Constructs a `Debug` instance for a pair of a key and a value given
   * `Debug` instances for the key and value types.
   */
  def keyValueDebug[A: Debug, B: Debug]: Debug[(A, B)] =
    n => Repr.KeyValue(n._1.debug, n._2.debug)

  /**
   * Constructs a `Debug[A]` from a function that converts a value of type `A`
   * to a `Repr`.
   */
  def make[A](f: A => Debug.Repr): Debug[A] =
    f(_)

  /**
   * Derives a `Debug[Array[A]]` given a `Debug[A]`.
   */
  implicit def ArrayDebug[A: Debug]: Debug[Array[A]] =
    array => Repr.VConstructor(List("scala"), "Array", array.map(_.debug).toList)

  /**
   * The `Debug` instance for `BigDecimal`.
   */
  implicit val BigDecimalDebug: Debug[BigDecimal] =
    bigDecimal =>
      Repr.VConstructor(List("scala", "math"), "BigDecimal", List(bigDecimal.toString.debug, bigDecimal.mc.debug))

  /**
   * The `Debug` instance for `BigInt`.
   */
  implicit val BigIntDebug: Debug[BigInt] =
    bigInt => Repr.VConstructor(List("scala", "math"), "BigInt", List(bigInt.toString.debug))

  /**
   * The `Debug` instance for `Boolean`.
   */
  implicit val BooleanDebug: Debug[Boolean] =
    Repr.Boolean(_)

  /**
   * The `Debug` instance for `Byte`.
   */
  implicit val ByteDebug: Debug[Byte] =
    Repr.Byte(_)

  /**
   * The `Debug` instance for `Char`.
   */
  implicit val CharDebug: Debug[Char] =
    Repr.Char(_)

  /**
   * Derives a `Debug[Chunk[A]]` given a `Debug[A]`.
   */
  implicit def ChunkDebug[A: Debug]: Debug[Chunk[A]] =
    chunk => Repr.VConstructor(List("zio"), "Chunk", chunk.map(_.debug).toList)

  /**
   * Derives a `Debug[F[A]]` given a `Derive[F, Debug]` and a `Debug[A]`.
   */
  implicit def DeriveDebug[F[_], A](implicit derive: Derive[F, Debug], debug: Debug[A]): Debug[F[A]] =
    derive.derive(debug)

  /**
   * The `Debug` instance for `Double`.
   */
  implicit val DoubleDebug: Debug[Double] =
    Repr.Double(_)

  /**
   * The `Debug` instance for `scala.concurrent.Duration`.
   */
  implicit val DurationScalaDebug: Debug[ScalaDuration] = {
    val namespace            = List("scala", "concurrent", "duration")
    val constructor          = "Duration"
    val namespaceConstructor = namespace ++ List(constructor)

    {
      case ScalaDuration.Zero      => Repr.Object(namespaceConstructor, "Zero")
      case ScalaDuration.Inf       => Repr.Object(namespaceConstructor, "Inf")
      case ScalaDuration.MinusInf  => Repr.Object(namespaceConstructor, "MinusInf")
      case ScalaDuration.Undefined => Repr.Object(namespaceConstructor, "Undefined")
      case d                       =>
        val (length, unit) = nanosToPrettyUnit(d.toNanos)
        Repr.Constructor(
          namespace,
          constructor,
          ("length", Repr.Long(length)),
          ("unit", unit.debug)
        )
    }
  }

  /**
   * The `Debug` instance for `zio.duration.Duration`.
   */
  implicit val DurationZIODebug: Debug[ZIODuration] = {
    val namespace            = List("zio", "duration")
    val constructor          = "Duration"
    val namespaceConstructor = namespace ++ List(constructor)

    {
      case ZIODuration.Zero     => Repr.Object(namespaceConstructor, "Zero")
      case ZIODuration.Infinity => Repr.Object(namespaceConstructor, "Infinity")
      case d                    =>
        val (amount, unit) = nanosToPrettyUnit(d.toNanos)
        Repr.Constructor(
          namespace,
          constructor,
          ("amount", Repr.Long(amount)),
          ("unit", unit.debug)
        )
    }
  }

  /**
   * Derives a `Debug[Either[E, A]]` given a `Debug[E]` and a `Debug[A]`.
   */
  implicit def EitherDebug[E: Debug, A: Debug]: Debug[Either[E, A]] = {
    case Left(e)  => Repr.VConstructor(List("scala"), "Left", List(e.debug))
    case Right(a) => Repr.VConstructor(List("scala"), "Right", List(a.debug))
  }

  /**
   * The `Debug` instance for `Float`.
   */
  implicit val FloatDebug: Debug[Float] =
    Repr.Float(_)

  /**
   * The `Debug` instance for `Int`.
   */
  implicit val IntDebug: Debug[Int] =
    Repr.Int(_)

  /**
   * Derives a `Debug[List[A]]` given a `Debug[A]`.
   */
  implicit def ListDebug[A: Debug]: Debug[List[A]] =
    list => Repr.VConstructor(List("scala"), "List", list.map(_.debug))

  /**
   * The `Debug` instance for `Long`.
   */
  implicit val LongDebug: Debug[Long] =
    Repr.Long(_)

  /**
   * The `Debug` instance for `java.math.MathContext`.
   */
  implicit val MathContextDebug: Debug[java.math.MathContext] =
    mc => Repr.VConstructor(List("java", "math"), "MathContext", List(mc.getPrecision.debug, mc.getRoundingMode.debug))

  /**
   * Derives a `Debug[Map[K, V]]` given a `Debug[K]` and a `Debug[V]`.
   */
  implicit def MapDebug[K: Debug, V: Debug]: Debug[Map[K, V]] =
    map => Repr.VConstructor(List("scala"), "Map", map.map(_.debug(keyValueDebug)).toList)

  /**
   * Derives a `Debug[NonEmptyChunk[A]]` given a `Debug[A]`.
   */
  implicit def NonEmptyChunkDebug[A: Debug]: Debug[NonEmptyChunk[A]] =
    nonEmptyChunk => Repr.VConstructor(List("zio"), "NonEmptyChunk", nonEmptyChunk.map(_.debug).toList)

  /**
   * The `Debug`instance for `Nothing`. Note that since there are no values of
   * type `Nothing` this `Debug` instance can never be called.
   */
  implicit val NothingDebug: Debug[Nothing] =
    n => n

  /**
   * Derives a `Debug[Option[A]]` given a `Debug[A]`.
   */
  implicit def OptionDebug[A: Debug]: Debug[Option[A]] = {
    case None    => Repr.Object(List("scala"), "None")
    case Some(a) => Repr.VConstructor(List("scala"), "Some", List(a.debug))
  }

  /**
   * The `Debug` instance for `java.math.RoundingMode`.
   */
  implicit val RoundingModeDebug: Debug[java.math.RoundingMode] = {
    case java.math.RoundingMode.CEILING     => Repr.Object(List("java", "math"), "RoundingMode.CEILING")
    case java.math.RoundingMode.DOWN        => Repr.Object(List("java", "math"), "RoundingMode.DOWN")
    case java.math.RoundingMode.FLOOR       => Repr.Object(List("java", "math"), "RoundingMode.FLOOR")
    case java.math.RoundingMode.HALF_DOWN   => Repr.Object(List("java", "math"), "RoundingMode.HALF_DOWN")
    case java.math.RoundingMode.HALF_EVEN   => Repr.Object(List("java", "math"), "RoundingMode.HALF_EVEN")
    case java.math.RoundingMode.HALF_UP     => Repr.Object(List("java", "math"), "RoundingMode.HALF_UP")
    case java.math.RoundingMode.UNNECESSARY => Repr.Object(List("java", "math"), "RoundingMode.UNNECESSARY")
    case java.math.RoundingMode.UP          => Repr.Object(List("java", "math"), "RoundingMode.UP")
  }

  /**
   * The `Debug` instance for `Short`.
   */
  implicit val ShortDebug: Debug[Short] =
    Repr.Short(_)

  /**
   * The `Debug` instance for `String`.
   */
  implicit val StringDebug: Debug[String] =
    Repr.String(_)

  /**
   * The `Debug` instance for `TimeUnit`.
   */
  implicit val TimeUnitDebug: Debug[TimeUnit] = tu =>
    Repr.Object(
      List("java", "util", "concurrent", "TimeUnit"),
      tu match {
        case TimeUnit.NANOSECONDS  => "NANOSECONDS"
        case TimeUnit.MICROSECONDS => "MICROSECONDS"
        case TimeUnit.MILLISECONDS => "MILLISECONDS"
        case TimeUnit.SECONDS      => "SECONDS"
        case TimeUnit.MINUTES      => "SECONDS"
        case TimeUnit.HOURS        => "HOURS"
        case TimeUnit.DAYS         => "DAYS"
      }
    )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple2Debug[A: Debug, B: Debug]: Debug[(A, B)] =
    tup2 => Repr.VConstructor(List("scala"), "Tuple2", List(tup2._1.debug, tup2._2.debug))

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple3Debug[A: Debug, B: Debug, C: Debug]: Debug[(A, B, C)] =
    tuple => Repr.VConstructor(List("scala"), "Tuple3", List(tuple._1.debug, tuple._2.debug, tuple._3.debug))

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple4Debug[A: Debug, B: Debug, C: Debug, D: Debug]: Debug[(A, B, C, D)] =
    tuple =>
      Repr.VConstructor(List("scala"), "Tuple4", List(tuple._1.debug, tuple._2.debug, tuple._3.debug, tuple._4.debug))

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple5Debug[A: Debug, B: Debug, C: Debug, D: Debug, E: Debug]: Debug[(A, B, C, D, E)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple5",
        List(tuple._1.debug, tuple._2.debug, tuple._3.debug, tuple._4.debug, tuple._5.debug)
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple6Debug[A: Debug, B: Debug, C: Debug, D: Debug, E: Debug, F: Debug]: Debug[(A, B, C, D, E, F)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple6",
        List(tuple._1.debug, tuple._2.debug, tuple._3.debug, tuple._4.debug, tuple._5.debug, tuple._6.debug)
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple7Debug[A: Debug, B: Debug, C: Debug, D: Debug, E: Debug, F: Debug, G: Debug]
    : Debug[(A, B, C, D, E, F, G)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple7",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple8Debug[A: Debug, B: Debug, C: Debug, D: Debug, E: Debug, F: Debug, G: Debug, H: Debug]
    : Debug[(A, B, C, D, E, F, G, H)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple8",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple9Debug[A: Debug, B: Debug, C: Debug, D: Debug, E: Debug, F: Debug, G: Debug, H: Debug, I: Debug]
    : Debug[(A, B, C, D, E, F, G, H, I)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple9",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple10Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple10",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple11Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple11",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple12Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple12",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple13Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple13",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple14Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple14",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple15Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple15",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple16Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple16",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple17Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug,
    Q: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple17",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug,
          tuple._17.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple18Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug,
    Q: Debug,
    R: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple18",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug,
          tuple._17.debug,
          tuple._18.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple19Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug,
    Q: Debug,
    R: Debug,
    S: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple19",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug,
          tuple._17.debug,
          tuple._18.debug,
          tuple._19.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple20Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug,
    Q: Debug,
    R: Debug,
    S: Debug,
    T: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple20",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug,
          tuple._17.debug,
          tuple._18.debug,
          tuple._19.debug,
          tuple._20.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple21Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug,
    Q: Debug,
    R: Debug,
    S: Debug,
    T: Debug,
    U: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple21",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug,
          tuple._17.debug,
          tuple._18.debug,
          tuple._19.debug,
          tuple._20.debug,
          tuple._21.debug
        )
      )

  /**
   * Derives an `Debug` for a product type given an `Debug` for each element of
   * the product type.
   */
  implicit def Tuple22Debug[
    A: Debug,
    B: Debug,
    C: Debug,
    D: Debug,
    E: Debug,
    F: Debug,
    G: Debug,
    H: Debug,
    I: Debug,
    J: Debug,
    K: Debug,
    L: Debug,
    M: Debug,
    N: Debug,
    O: Debug,
    P: Debug,
    Q: Debug,
    R: Debug,
    S: Debug,
    T: Debug,
    U: Debug,
    V: Debug
  ]: Debug[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    tuple =>
      Repr.VConstructor(
        List("scala"),
        "Tuple22",
        List(
          tuple._1.debug,
          tuple._2.debug,
          tuple._3.debug,
          tuple._4.debug,
          tuple._5.debug,
          tuple._6.debug,
          tuple._7.debug,
          tuple._8.debug,
          tuple._9.debug,
          tuple._10.debug,
          tuple._11.debug,
          tuple._12.debug,
          tuple._13.debug,
          tuple._14.debug,
          tuple._15.debug,
          tuple._16.debug,
          tuple._17.debug,
          tuple._18.debug,
          tuple._19.debug,
          tuple._20.debug,
          tuple._21.debug,
          tuple._22.debug
        )
      )

  /**
   * The `Debug` instance for `Unit`.
   */
  implicit val UnitDebug: Debug[Unit] =
    _ => Repr.Object("scala" :: Nil, "()")

  /**
   * Derives a `Debug[Vector[A]]` given a `Debug[A]`.
   */
  implicit def VectorDebug[A: Debug]: Debug[Vector[A]] =
    vector => Repr.VConstructor(List("scala"), "Vector", vector.map(_.debug).toList)

  private val nanosToPrettyUnit: Long => (Long, TimeUnit) = {
    val ns_per_us  = 1000L
    val ns_per_ms  = ns_per_us * 1000
    val ns_per_s   = ns_per_ms * 1000
    val ns_per_min = ns_per_s * 60
    val ns_per_h   = ns_per_min * 60
    val ns_per_d   = ns_per_h * 24

    (nanos: Long) =>
      import java.util.concurrent.TimeUnit._
      if (nanos % ns_per_d == 0) (nanos / ns_per_d, DAYS)
      else if (nanos % ns_per_h == 0) (nanos / ns_per_h, HOURS)
      else if (nanos % ns_per_min == 0) (nanos / ns_per_min, MINUTES)
      else if (nanos % ns_per_s == 0) (nanos / ns_per_s, SECONDS)
      else if (nanos % ns_per_ms == 0) (nanos / ns_per_ms, MILLISECONDS)
      else if (nanos % ns_per_us == 0) (nanos / ns_per_us, MICROSECONDS)
      else (nanos, NANOSECONDS)
  }
}

trait DebugSyntax {
  implicit class DebugOps[A](self: A) {
    def debug(implicit debug: Debug[A]): Debug.Repr = debug.debug(self)
    def render(implicit debug: Debug[A]): String    = debug.render(self)
  }

  implicit final class DebugInterpolator(_sc: StringContext) {
    def d(args: Debug.Repr*): String = _sc.s(args.map(_.render): _*)
  }
}
