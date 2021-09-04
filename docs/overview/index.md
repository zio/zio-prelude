---
id: overview_index
title: "Summary"
---

ZIO Prelude is a library focused on providing a core set of functional data types and abstractions that can help you solve a variety of day to day problems. The tools provided by ZIO Prelude fall into the following main categories:

- **[Functional Abstractions](functionalabstractions/index.md)** - Functional abstractions to describe different ways of combining data, making it easy for you to combine complex data types in a principled way.
- **[Functional Data Types](functionaldatatypes/index.md)** - Additional data types to supplement the ones in the Scala standard library such as `Validation` and `NonEmptyList` to enable more accurate domain modeling and handle common problems like data validation.
- **[New Types](newtypes/index.md)** - Zero overhead newtypes and refined newtypes to allow you to increase the type safety of your code base with zero overhead and minimal boilerplate.
- **[ZPure](zpure/index.md)** - A description of a computation that supports logging, context, state, and errors, providing all the functionality traditionally offered by monad transformers with dramatically better performance and ergonomics.

## Installation

Include ZIO Prelude in your project by adding the following to your build.sbt file:

```scala mdoc:passthrough
println(s"""```""")
if (zio.prelude.BuildInfo.isSnapshot)
  println(s"""resolvers += Resolver.sonatypeRepo("snapshots")""")
println(s"""libraryDependencies += "dev.zio" %% "zio-prelude" % "${zio.prelude.BuildInfo.version}"""")
println(s"""```""")
```
