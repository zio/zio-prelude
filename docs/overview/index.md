---
id: overview_index
title: "Summary"
---

ZIO Prelude is an alternative approach to functional abstractions in Scala, which throws out the classic functor hierarchy in favor of a modular algebraic approach that is smaller, easier to understand and teach, and more expressive.

The ZIO Prelude Microsite is currently under development. In the meantime you can check tthe following resources:

- [Scaladoc of zio-prelude](https://zio.github.io/zio-prelude/api/index.html)
- [Github README.md](https://github.com/zio/zio-prelude)
- [Type Class diagrams](https://zio.github.io/zio-prelude/docs/overview/overview_diagrams)
- [Refactoring Functional Type Classes](https://www.slideshare.net/jdegoes/refactoring-functional-type-classes) - slides by John De Goes and Adam Fraser,
- [SF Scala: Reimagining Functional Type Classes](https://youtu.be/OwmHgL9F_9Q) - a talk by John A. De Goes and Adam Fraser
- [The Terror-Free Guide to Introducing Functional Scala at Work](https://www.slideshare.net/jv2301/the-terrorfree-guide-to-introducing-functional-scala-at-work) - slides by Jorge Vásquez,
- [Exploring ZIO Prelude The game-changer for type classes in Scala](https://youtu.be/OzoMofqsPg8) - talk by Jorge Vásquez
- [What's Ap with ZIO Prelude?](https://justinhj.github.io/2020/08/02/whats-ap-with-zio-prelude.html) - an article by justinhj

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
