/*
 * Copyright 2020-2022 John A. De Goes and the ZIO Contributors
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

/**
 * The `Newtype` module provides functionality for creating zero overhead
 * newtypes. Newtypes wrap an existing type and have the same representation as
 * the underlying type at runtime but are treated as distinct types by the
 * Scala compiler. Newtypes can be used to increase type safety in modeling a
 * domain, for example by creating separate types for `Meter` and `Foot`.
 * Additionally, Smart Newtypes allow for compile-time validation of values,
 * e.g., ensuring an Int is 4 digits long (a PIN) or that a String matches a
 * particular Regex (an Email).
 *
 * Newtypes can also be used to provide coherent instances for types that can
 * support more than one version of a typeclass. For example, the `And` and
 * `Or` types can be used to distinguish boolean conjunction and disjunction.
 *
 * To create a newtype, simply implement an object which extends `Newtype[A]`,
 * where `A` is the underlying type you are wrapping. Then implement a type
 * alias for `Type` in the object you create.
 *
 * {{{
 * object Meter extends Newtype[Double]
 * type Meter = Meter.Type
 * }}}
 *
 * This creates a newtype `Meter` that wraps an underlying `Double` value.
 *
 * We can convert the underlying type to the newtype using the `wrap` method on
 * the object we created, or convert a collection of values of the underlying
 * type using `wrapAll`. Similarly we can convert a newtype back to the
 * underlying value using `unwrap` and `unwrapAll`. We can also wrap a value in
 * a newtype using the `apply` method of the object we created or pattern match
 * to recover the underlying value using its `unapply` method. None of these
 * methods incur any runtime overhead in doing these conversions.
 *
 * {{{
 * implicit class MeterSyntax(private val self: Meter) extends AnyVal {
 *   def +(that: Meter): Meter =
 *     Meter.wrap(Meter.unwrap(self) + Meter.unwrap(that))
 * }
 *
 * val x = Meter(3.4)
 * val y = Meter(4.3)
 * val z = x + y
 * }}}
 *
 * In the example above, `Meter` is completely independent of `Double`. Outside
 * of the methods we create to convert between them, the two types are
 * completely unrelated from the perspective of the Scala compiler. This can be
 * valuable because it fully hides the underlying type. For example, we cannot
 * accidentally add a `Meter` and a `Foot`. On the other hand `Meter` also does
 * not have any of the methods defined on it that are defined for `Double`
 * values so we have to re-implement them ourselves or wrap and unwrap each
 * time.
 *
 * It is also possible to create newtypes that are subtypes of existing types.
 * To do this, just implement your newtype the same we as described above but
 * extends `Subtype[A]`, where `A` is the underlying value you are wrapping.
 *
 * {{{
 * object And extends Subtype[Boolean]
 * type And = And.Type
 *
 * object Or extends Subtype[Boolean]
 * type Or = Or.Type
 *
 * implicit object AndInstance extends Associative[And] with Identity[And] {
 *   def combine(l: => And, r: => And): And =
 *     And(l && r)
 *   val identity: And =
 *     And(true)
 * }
 *
 * implicit object OrInstance extends Associative[Or] with Identity[Or] {
 *   def combine(l: => Or, r: => Or): Or =
 *     Or(l || r)
 *   val identity: Or =
 *     Or(false)
 * }
 *
 * def foldMap[A, B](as: List[A])(f: A => B)(implicit B: Associative[B] with Identity[B]) =
 *   as.foldLeft(B.identity)((b, a) => B.combine(b, f(a)))
 *
 * def exists[A](as: List[A])(f: A => Boolean): Boolean =
 *   Or.unwrap(foldMap(as)(a => Or(f(a))))
 *
 * def forall[A](as: List[A])(f: A => Boolean): Boolean =
 *   And.unwrap(foldMap(as)(a => And(f(a))))
 * }}}
 *
 * Notice how `And` and `Or` are distinct types that each can have their own
 * coherent typeclass instances, but operations on `Boolean` such as `&&` and
 * `||` are still available on them. These methods can still return the
 * underlying type, so this technique is most useful for creating coherent
 * typeclasses instances rather than type safe domain specific languages.
 *
 * Finally, it is possible to create Smart Newtypes that can be validated at
 * compile-time when constructed with a literal value, or at run-time with a
 * dynamic value, returning a [[Validation]]. In this case we must define an
 * additional `def assertion` method on the Object, indicating how to validate
 * an instance of the underlying type. For example, let's create a newtype for
 * natural numbers, which must be equal to or greater than zero. (Note that the
 * syntax differs between Scala 2 and 3 due to changes in the macro API).
 *
 * {{{
 * import zio.prelude.Assertion.greaterThanOrEqualTo
 *
 * type Natural = Natural.Type
 * object Natural extends Newtype[Int] {
 *   // Scala 2 — be sure you DO NOT give a type annotation to `assertion`
 *   def assertion = assert {
 *     greaterThanOrEqualTo(0)
 *   }
 *
 *   // Scala 3
 *   override inline def assertion: Assertion[Int] =
 *     greaterThanOrEqualTo(0)
 * }
 * }}}
 *
 * In this case, attempting to convert an integer to a natural number will
 * return a `Natural` that will either return a`Natural` if the integer was
 * equal to or greater than zero or a fail to compile with a descriptive error
 * message if the condition was not satisfied.
 *
 * {{{
 *   val compiles = Natural(10)
 *   val fails    = Natural(-99) // -99 does not satisfy greaterThanOrEqualTo(0)
 * }}}
 *
 * If you need to supply a variable or other run-time validation to a Smart
 * Newtype, use the `make` method instead, which will return a [[Validation]].
 *
 * {{{
 *   val result: Validation[String, Natural] = Natural.make(10)
 * }}}
 */

/**
 * The class of objects corresponding to subtypes. Users should implement an
 * object that extends this class to create their own subtypes, specifying
 * `A` as the underlying type to wrap.
 *
 * {{{
 * object And extends Subtype[Boolean]
 * type And = And.Type
 * }}}
 */
abstract class Subtype[A] extends Newtype[A] {
  type Base <: A
}
