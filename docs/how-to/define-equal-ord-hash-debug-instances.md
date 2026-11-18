---
id: define-equal-ord-hash-debug-instances
title: "How to Define Equal, Ord, Hash and Debug Instances"
description: "Define Equal, Ord, Hash and Debug instances for your own data types, and choose which fields count."
keywords:
  - "Type Class Instances"
  - "Equality"
  - "Ordering"
  - "Equal"
  - "Ord"
  - "Hash"
  - "Debug"
---

In this guide, we define `Equal`, `Ord`, `Hash` and `Debug` instances for a `Version` data type. By the end, you will have a type that compares by its release numbers only, sorts correctly, hashes consistently, and prints in a readable form.

We take an incremental approach: first equality, then ordering and hashing together, and finally debugging output.

## The Problem

Case classes compare all of their fields, and the standard library has no notion of an equality that ignores some of them. When two values should be the same by a domain rule, the built-in `==` disagrees with that rule.

For example, a release version has a `label` that records how the build was cut. Two builds with the same major and minor numbers are the same release, but `==` compares the label as well:

```scala mdoc:compile-only
final case class Version(major: Int, minor: Int, label: String)

val candidate = Version(1, 2, "rc")
val finalBuild = Version(1, 2, "final")

val sameRelease = candidate == finalBuild
```

Here `sameRelease` is `false` even though both values describe release 1.2. Writing `equals`, `hashCode` and an `Ordering` by hand for every such type is repetitive, and nothing ties the three together: it is easy to make `equals` ignore the label while `hashCode` still includes it.

In this guide, we solve this by building the instances from smaller ones with `contramap`.

## Prerequisites

We need the `zio-prelude` dependency:

```scala
libraryDependencies += "dev.zio" %% "zio-prelude" % "@VERSION@"
```

We import the package once for the whole guide, and define the type we work with. The `Version` type is the same as in the problem above:

```scala mdoc:silent:reset
import zio.prelude._
import zio.prelude.coherent.HashOrd

final case class Version(major: Int, minor: Int, label: String)
```

## Defining Equality

`Equal[A]` is the evidence that two `A` values can be compared. The most direct way to define one for our own type is to reuse the instance of a simpler type, and say how to get from our type to it. `Equal#contramap` takes a function from our type to the simpler type, and compares the results.

We compare versions by the pair of major and minor numbers. `Equal[(Int, Int)]` exists because ZIO Prelude provides `Equal` instances for tuples of types that have one:

```scala mdoc:silent
object VersionEqual {
  implicit val equal: Equal[Version] =
    Equal[(Int, Int)].contramap(version => (version.major, version.minor))
}

import VersionEqual._
```

With the instance in scope, the `===` operator uses it:

```scala mdoc
Version(1, 2, "rc") === Version(1, 2, "final")

Version(1, 2, "rc") === Version(2, 0, "final")
```

When there is no simpler type to reuse, `Equal.make` builds an instance from a function that takes two values and returns whether they are equal. For types where the standard `equals` is already right, `Equal.default` wraps it.

## Defining Ordering and Hashing Together

`Ord[A]` orders values, and `Hash[A]` hashes them. Both are also an `Equal[A]`, so an implicit `Equal`, `Ord` and `Hash` for the same type in the same scope make `Equal` ambiguous for the compiler. We avoid that by defining a single instance that is both a `Hash` and an `Ord`, using `HashOrd.make`. Because that instance is an `Equal` as well, we do not define a separate `Equal` any more.

`HashOrd.make` takes a hash function and an ordering function. The equality of the instance follows from the ordering. We reuse the instances of the `(Int, Int)` key for both functions, so they cannot disagree about which fields count:

```scala mdoc:silent:reset
import zio.prelude._
import zio.prelude.coherent.HashOrd

final case class Version(major: Int, minor: Int, label: String)

object Version {
  private def key(version: Version): (Int, Int) =
    (version.major, version.minor)

  implicit val hashOrd: Hash[Version] with Ord[Version] =
    HashOrd.make(
      version => key(version).hash,
      (left, right) => key(left) =?= key(right)
    )
}
```

The code above:

- `key` — extracts the fields that define a release, in one place
- `key(version).hash` — hashes the key with the `Hash` instance for tuples
- `key(left) =?= key(right)` — compares two keys and returns an `Ordering`
- `Hash[Version] with Ord[Version]` — the one implicit that provides `Equal`, `Hash` and `Ord`

Next, we use the operators the instances give us. `===` checks equality, `<` and its relatives compare, `=?=` returns the `Ordering`, and `hash` hashes:

```scala mdoc
val candidate  = Version(1, 2, "rc")
val finalBuild = Version(1, 2, "final")
val next       = Version(2, 0, "final")

candidate === finalBuild

candidate < next

candidate =?= next

candidate.hash == finalBuild.hash
```

The last line prints `true`, as the consistency law requires: values that are equal must have equal hashes. To sort a list, we hand the instance to the standard library with `Ord#toScala`:

```scala mdoc
List(next, candidate, finalBuild).sorted(Ord[Version].toScala)
```

Notice that the sort is stable, so `candidate` stays before `finalBuild`: they compare as equal.

:::warning
The standard collections do not use `Equal` or `Hash`. `Set(candidate, finalBuild).size` and `List#distinct` still use `equals` and `hashCode`, so they treat these two versions as different. Pass the instance explicitly where a standard collection needs an ordering, as `Ord#toScala` does above.
:::

## Defining Debug Output

`Debug[A]` renders a value for humans. Instead of building a `String`, an instance builds a structured `Debug.Repr`, which a renderer then turns into text. We construct the representation with `Debug.make`. For a class with named fields, `Debug.Repr.Constructor` takes a namespace, a name, and the fields as name and representation pairs. We get the representation of each field from its own instance with the `debug` operator:

```scala mdoc:silent
import zio.prelude.Debug.Repr

implicit val versionDebug: Debug[Version] =
  Debug.make { version =>
    Repr.Constructor(
      List("example"),
      "Version",
      "major" -> version.major.debug,
      "minor" -> version.minor.debug,
      "label" -> version.label.debug
    )
  }
```

The `render` operator uses the `Simple` renderer, and a renderer can be passed explicitly. The `d` string interpolator renders each argument with its own instance:

```scala mdoc
candidate.render

candidate.debug.render(Debug.Renderer.Scala)

candidate.debug.render(Debug.Renderer.Full)

d"released as $candidate"
```

The `Scala` renderer produces text that can be pasted back into code, and the `Full` renderer includes the namespace. A type with several cases, such as a sealed trait, gets a `Debug` that pattern matches on the case and returns `Repr.VConstructor` or `Repr.Object` for each. For case classes, the [Magnolia guide](./derive-debug-with-magnolia.md) shows how to derive the whole instance.

## Going Further

- Test that your instances obey the laws in [How to Test Instances with Laws](./test-instances-with-laws.md).
- Read the reference pages for [Equal](../functional-abstractions/concrete-types/equal.md), [Ord](../functional-abstractions/concrete-types/ord.md), [Hash](../functional-abstractions/concrete-types/hash.md) and [Debug](../functional-abstractions/concrete-types/debug.md).
