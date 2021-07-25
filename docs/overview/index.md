---
id: overview_index
title: "Summary"
---

ZIO Prelude is a library focused on providing a core set of functional data types and abstractions that can help you solve a variety of day to day problems. The tools provided by ZIO Prelude fall into the following main categories:

- **Functional Abstractions** - Functional abstractions to describe different ways of combining data, making it easy for you to combine complex data types in a principled way.
- **Functional Data Types** - Additional data types to supplement the ones in the Scala standard library such as `Validation` and `NonEmptyList` to enable more accurate domain modeling and handle common problems like data validation.
- **New Types** - Zero overhead new types to allow you to increase the type safety of your code base with zero overhead and minimal boilerplate.
- **ZPure** - A description of a computation that supports logging, context, state, and errors, providing all the functionality traditionally offered by monad transformers with dramatically better performance and ergonomics.

## Installation

In general, to add ZIO Prelude to your project, follow the instructions given by your build tool. 

As a convenience, we've added sample configurations for the following build tools:

### SBT
```
libraryDependencies += "dev.zio" %% "zio-prelude" % "1.0.0-RC5"
```

### Gradle
```
implementation group: 'dev.zio', name: 'zio-prelude_2.13', version: '1.0.0-RC5'
```

respectively for Scala 3

```
implementation group: 'dev.zio', name: 'zio-prelude_3', version: '1.0.0-RC5'
```

### Maven
```xml
<dependency>
    <groupId>dev.zio</groupId>
    <artifactId>zio-prelude_2.13</artifactId>
    <version>1.0.0-RC5</version>
</dependency>
```

respectively for Scala 3

```xml
<dependency>
    <groupId>dev.zio</groupId>
    <artifactId>zio-prelude_3</artifactId>
    <version>1.0.0-RC5</version>
</dependency>
```
