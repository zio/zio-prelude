---
id: overview_phantomtypes
title: "Phantom Types"
---


ZIO-Prelude contains so-called "Phantom Types". 

Simplified, Phantom Types are types that are only visible during compilation, but disappear during runtime and therefor have no runtime overhead. 

# Intuition

You probably already know the principle of phantom types from code like this, creating [Value Classes](https://docs.scala-lang.org/overviews/core/value-classes.html):
```scala
case class MyId(value: Long) extends AnyVal
```

While most of the time the Scala compiler is able to optimize away the `MyId`, sometimes it can't and unnecessary "boxing" (= object allocation) happens
at Runtime, hurting the performance of your application. 

In ZIO-Prelude, we have two Phantom types - `Newtype` and `Subtype` - that make sure that no boxing occurs at runtime, yet give you compile time safety while
developing.

# How to create a Phantom Type

You define your own phantom types like this, usually in a `package object`:

```scala

package com.acme.myapp

import java.util.UUID

package domain {

    object CorrelationId extends zio.prelude.Subtype[UUID] {
        def random: CorrelationId = CorrelationId(UUID.randomUUID())
    }
    type CorrelationId = CorrelationId.Type

    object UserId extends zio.prelude.Newtype[String]
    type UserId = UserId.Type
}

```

# Difference between `Subtype` and `Newtype` 

As you've seen, defining phantom types with ZIO-Prelude is pretty straight-forward. 
But what is the difference between `Subtype` and `Newtype` ? 

Simply speaking, you can use a `Subtype[A]` everywhere, where your program accepts an `A`, whereas an instance of a `NewType[A]` can only be used where exactly a `Newtype[A]` is specified. 

## Examples with Subtype

We can use a instance of `Subtype[A]` everywhere an `A` would be accepted:

### Usage as a parameter
```scala

object Example1 {
    import com.acme.myapp.domain._
    import java.util.UUID

    def logGenerally(correlationId: UUID, message: String): Unit = ???

    // this works, as CorrelationId is a "Subtype" of UUID
    logGenerally(CorrelationId.random, "test1") 

    // this of course still works
    logGenerally(UUID.randomUUID(), "test2")

}
```

This allows us to refactor our codebase, add type information and step by step make our code more strict:

```scala

object Example2 {
    import com.acme.myapp.domain._
    import java.util.UUID


    def logSpecific(correlationId: CorrelationId, message: String): Unit = ???
    // this works
    logSpecific(CorrelationId.random, "test1"))

    // this doesn't compile, as we're now more strict
    logSpecific(UUID.randomUUID(), "test2)

}
```

### Accessing underlying methods
If you have an instance of a `Subtype[A]`, you're able to access any method of the underlying type `A` is if the "wrapper" would not be there, e.g.

```scala

object Example3 {
    import com.acme.myapp.domain._
    import java.util.UUID

    val c = CorrelationId(UUID.randomUUID())
    println(s"variant of UUID = ${c.variant}")
    println(s"clockSequence of UUID = ${c.clockSequence}")
    println(s"getLeastSignificantBits of UUID = ${c.getLeastSignificantBits}")
    println(s"getMostSignificantBits of UUID = ${c.getMostSignificantBits}")

    // toString also delegates to the underlying object
    println(c)
    println(c.toString)

}
```

## Examples with Newtype

We can only use a instance of `Newtype[A]` where such an instance is explicitely specified:

```scala

object Example4 {
    import com.acme.myapp.domain._
    import java.util.UUID

    
    def traceUser(userId: UserId, message: String): Unit = ???

    // reminder: object UserId extends zio.prelude.Newtype[String] 

    // this works
    traceUser(UserId("michelle"), "has entered the room") 

    // this doesn't, as we require an instance of UserId
    traceuser("michelle", "leaves the room")
}
```

Also, we can't directly access methods of the underlying type. To enable access, we have to `unwrap` the value.
Again, this gets optimized away during compilation, so no runtime overhead!

```scala

object Example5 {
    import com.acme.myapp.domain._
    import java.util.UUID

    val user = UserId("nouschin")

    // this doesn't compile
    println(userId.length)

    // this does
    println(UserId.unwrap(user).length)
    
}
```

## Wrapping and unwrapping

There are a few best practices when it comes to working with Prelude types:

```scala

object Example6 {

    object MW extends zio.prelude.Subtype[Double] {
        val zero: MW = MW(0.0d)
    }
    type MW = MW.Type

    val singlePower = MW(5.0d) // 5 megawatt
    
    // as Subtype allows access to the underlying methods, we have wrap on common operations
    val doublePower: Double = singlePower + singlePower // the + operator exposes the default return type of Double
    val doublePowerTyped: MW = MW( singlePower + singlePower ) 
    
    // handling collections:

    // don't do this:
    val badExample = List(3, 4, 5).map(MW(_)) // List( MW(3.0), MW(4.0), MW(5.0))
    // ->  this would be translated to 
    val badExample = List(3, 4, 5).map(identity) // which the compiler cannot optimize away

    // instead do this
    val listOfPowers = MW.wrapAll(List(3, 4, 5)) // List( MW(3.0), MW(4.0), MW(5.0))

    // same for other wrappers:
    val maybePower = MW.wrapAll(Option(5.0)) // Option[MW]

    // make use of constants
    val isZeroPower = MW.zero == MW(0) // true
    val isZeroPower2 = MW.zero == 0.0d // also true (as MW is a Subtype and goes away during runtime)


}
```

### Using Scala collections aggregate functions
For numerical data types, the Scala collections feature handy helper functions like `.sum`, `.min`, `.max` etc.
By default, the compiler is unable to figure out that your Phantom Type is the same as one that already has the
proper definitions for these aggregate functions, like e.g. a Double has. 

To make these available in your program, you can do something like this: 

```scala

object Example7 {

    object MW extends zio.prelude.Subtype[Double] {
        val zero: MW = MW(0.0d)
        implicit val numeric: Numeric[MW] = MW.wrapAll(implicitly[Numeric[Double]])
    }
    type MW = MW.Type

    val powers = MW.wrapAll(List(1,2,3,4,5)) // List( MW(1), MW(2), MW(3), MW(4), MW(5))
    val sum = powers.sum // MW(15)
    val min = powers.min // MW(1)
    val max = powers.max // MW(5)

}
```

