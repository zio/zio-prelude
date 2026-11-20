---
id: assertions
title: "Assertions"
---

An `Assertion[A]` is a composable predicate over values of type `A` that ZIO Prelude can evaluate both at run time and, for literal values, at compile time. Assertions are what [smart newtypes](./smart-newtypes.md) use to describe which values of the underlying type are valid.

This page documents the `Assertion` and `AssertionError` types themselves: the predefined assertions, how to combine them, how to run them directly, and how failures are reported. Both types live in the `zio-prelude-macros` module, which `zio-prelude` depends on, so a project that depends on `zio-prelude` already has them.

## Installation

To use assertions on their own, or to depend on the macros module without the rest of the library, add it to the build:

```scala
// Macros behind smart newtypes and assertions. JVM, Scala.js, Native.
libraryDependencies += "dev.zio" %% "zio-prelude-macros" % "@VERSION@"
```

Most projects get the module transitively through `zio-prelude`.

## Running an Assertion

To check a value, call `Assertion#apply`. It returns `Right(())` when the value satisfies the assertion and `Left` with an `AssertionError` when it does not:

```scala mdoc
import zio.prelude.Assertion

val pin = Assertion.between(1000, 9999)

pin(1234)

pin(42)
```

Every assertion carries a short description of the condition it checks, such as `between(1000, 9999)`. That description is what ends up in error messages.

## Predefined Assertions

The `Assertion` companion object provides constructors for the most common checks. The ordering based assertions require an `Ordering` for the element type, and `Assertion.divisibleBy` and `Assertion.powerOf` require a `Numeric`.

| Assertion                                      | Satisfied when the value                         |
| ---------------------------------------------- | ------------------------------------------------ |
| `Assertion.anything`                           | is any value                                     |
| `Assertion.never`                              | is never satisfied                               |
| `Assertion.equalTo(value)`                     | equals `value`                                   |
| `Assertion.notEqualTo(value)`                  | does not equal `value`                           |
| `Assertion.between(min, max)`                  | is between `min` and `max`, both inclusive       |
| `Assertion.greaterThan(value)`                 | is greater than `value`                          |
| `Assertion.greaterThanOrEqualTo(value)`        | is greater than or equal to `value`              |
| `Assertion.lessThan(value)`                    | is less than `value`                             |
| `Assertion.lessThanOrEqualTo(value)`           | is less than or equal to `value`                 |
| `Assertion.divisibleBy(n)`                     | is divisible by `n`                              |
| `Assertion.powerOf(base)`                      | is a power of `base`                             |
| `Assertion.contains(string)`                   | is a string containing `string`                  |
| `Assertion.startsWith(prefix)`                 | is a string starting with `prefix`               |
| `Assertion.startsWithIgnoreCase(prefix)`       | is a string starting with `prefix`, ignoring case |
| `Assertion.endsWith(suffix)`                   | is a string ending with `suffix`                 |
| `Assertion.endsWithIgnoreCase(suffix)`         | is a string ending with `suffix`, ignoring case  |
| `Assertion.hasLength(lengthAssertion)`         | is a string whose length satisfies the assertion |
| `Assertion.isEmptyString`                      | is the empty string                              |
| `Assertion.matches(regex)`                     | is a string that matches the whole regex         |

Because `Assertion.hasLength` takes an assertion on the length, it composes with the numeric assertions. To require a string of three to eight characters:

```scala mdoc
val username = Assertion.hasLength(Assertion.between(3, 8))

username("zio")

username("zio-prelude-user")
```

## Combining Assertions

Assertions combine with the Boolean operators `Assertion#&&`, `Assertion#||` and `Assertion#unary_!`. To require a positive multiple of ten, we combine two assertions with `&&`:

```scala mdoc
val positiveMultipleOfTen = Assertion.greaterThan(0) && Assertion.divisibleBy(10)

positiveMultipleOfTen(30)

positiveMultipleOfTen(35)
```

The `||` operator succeeds when either side does, and `!` negates an assertion. The negation of a predefined assertion reports a matching negated description, so `!Assertion.between(1, 10)` fails with `notBetween(1, 10)` rather than with a generic message:

```scala mdoc
val outsideRange = !Assertion.between(1, 10)

outsideRange(5)
```

Two constants are provided for completeness: `Assertion.anything` always succeeds, and `Assertion.never` is defined as its negation and always fails.

## Matching Strings with Regular Expressions

`Assertion.matches` has three overloads. It accepts a plain `String` pattern, a `scala.util.matching.Regex`, or a value built with the `Assertion.Regex` combinators. The match must cover the whole string, as with `String#matches`:

```scala mdoc
val threeLettersTwoDigits = Assertion.matches("[a-z]{3}\\d\\d")

threeLettersTwoDigits("abc42")

threeLettersTwoDigits("abc42!")
```

The `Assertion.Regex` type is a small combinator language that compiles to a pattern string. It offers the constants `anyChar`, `alphanumeric`, `nonAlphanumeric`, `whitespace`, `nonWhitespace`, `digit`, `nonDigit`, `start` and `end`, and the constructors `literal`, `anyCharOf`, `anyRegexOf`, `notAnyCharOf`, `notAnyRegexOf`, `inRange` and `notInRange`. Instances combine with `~` for sequencing, `|` for alternatives, and the postfix repetition operators `*`, `+` and `?`, plus `between(min, max)`, `min(n)` and `max(n)`.

To build the same three letters followed by two digits pattern with combinators:

```scala mdoc
import zio.prelude.Assertion.Regex._

val viaCombinators = Assertion.matches(inRange('a', 'z').between(3, 3) ~ digit.between(2, 2))

viaCombinators("abc42")

viaCombinators("ab42")
```

## Assertion Errors

When an assertion fails, `Assertion#apply` returns an `AssertionError`. It has two cases: `AssertionError.Failure(condition)` for a single failed condition, and `AssertionError.Many` for several. Combining two failed assertions with `&&` or `||` collects both failures instead of reporting only the first:

```scala mdoc
val both = Assertion.lessThan(0) && Assertion.greaterThan(100)

val result = both(50)
```

To turn an error into human readable messages, call `AssertionError#toNel` with the rendered value. It returns a non-empty list of strings, one per failed condition:

```scala mdoc
result.left.map(_.toNel("50"))
```

The `AssertionError#render` method produces the colored, multi-line text that the compiler prints, and `AssertionError#++` concatenates two errors. We can also build an error by hand with `AssertionError.failure`, which is useful when writing a custom check:

```scala mdoc
import zio.prelude.AssertionError

val custom = AssertionError.failure("isPalindrome")

custom.toNel("hello")
```

## Compile-Time Checking

The reason assertions exist as data rather than as plain functions is that the macros can read them while compiling. When a [smart newtype](./smart-newtypes.md) declares an `assertion`, a call such as `Pin(1234)` with a literal argument runs the assertion during compilation, and a failing literal becomes a compile error that lists each failed condition. A value that is not a literal cannot be checked by the compiler, so it must go through the run-time `make` constructors instead.

The way the assertion is declared differs by Scala version. In Scala 2 the object defines `def assertion = assert(...)` and the type of `assertion` must not be annotated. In Scala 3 it is `override inline def assertion = ...`, and it can refer only to definitions that are themselves inline or that the compiler can evaluate. [Smart Newtypes](./smart-newtypes.md) shows both forms.

### Escape Hatch for Custom Predicates

When a predicate cannot be expressed with the predefined assertions, Scala 2 offers `Newtype#assertCustom`, which takes an arbitrary function from the underlying type to `Either[AssertionError, Unit]`. The macro evaluates the function at compile time, so it is subject to the limits of the Scala 2 macro evaluator, and the lambda parameter needs an explicit type. Prefer a predefined `Assertion` when one fits:

```scala mdoc:compile-only
import zio.prelude.{AssertionError, Newtype}

object Palindrome extends Newtype[String] {
  override def assertion = assertCustom { (str: String) =>
    if (str.reverse == str) Right(()) else Left(AssertionError.failure("isPalindrome"))
  }
}
```

In Scala 3 the equivalent is a `Validator`. See the [Validator](./validator.md) page.

## Notes and Limitations

An assertion is only evaluated at compile time for a literal argument. In Scala 2, passing a non-literal to `Newtype.apply` on an asserted newtype is a compile error, and in Scala 3 it reports that a literal value is required. Use the `make` and `makeAll` constructors for run-time values, and `wrap` inside the newtype object when you can prove validity yourself. `Newtype#wrapAll` is rejected in Scala 2 for a newtype that has an assertion.
