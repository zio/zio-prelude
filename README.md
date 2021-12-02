# ZIO Prelude

| Project Stage | CI | Release | Snapshot | Discord |
| --- | --- | --- | --- | --- |
| [![Project stage][Stage]][Stage-Page] | ![CI][Badge-CI] | [![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases] | [![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots] | [![Badge-Discord]][Link-Discord] |

# Summary

ZIO Prelude is an alternative approach to functional abstractions in Scala, which throws out the 
classic functor hierarchy in favor of a modular algebraic approach that is smaller, easier 
to understand and teach, and more expressive.

ZIO Prelude has three key areas of focus:

- Data structures, and type classes for traversing them. ZIO Prelude embraces the collections
in the Scala standard library, and extends them with new instances and new useful additions.
- Patterns of composition for types. ZIO Prelude provides a small catalog of patterns for binary 
operators, which combine two values into another value of the same type. These patterns are named 
after the algebraic laws they satisfy: associativity, commutativity, and identity.
- Patterns of composition for type constructors. ZIO Prelude provides a catalog of patterns for 
binary operators on type constructors (things like `Future`, `Option`, ZIO `Task`). These patterns 
are named after the algebraic laws they satisfy (associativity, commutativity, and identity) and the 
structure they produce, whether a tuple or an either. 

The library has a small research-stage package (`zio.prelude.fx`) that provides abstraction over 
expressive effect types like ZIO and `ZPure`.

# Documentation

Learn more on the [ZIO Prelude Microsite](https://zio.github.io/zio-prelude/)!

# Contributing

[Documentation for contributors](https://zio.github.io/zio-prelude/docs/about/about_contributing)

## Code of Conduct

See the [Code of Conduct](https://zio.github.io/zio-prelude/docs/about/about_coc)

## Support

Come chat with us on [![Badge-Discord]][Link-Discord].

# License

[License](LICENSE)

[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-prelude_2.12.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/dev.zio/zio-prelude_2.12.svg "Sonatype Snapshots"
[Badge-Discord]: https://img.shields.io/discord/629491597070827530?logo=discord "chat on discord"
[Badge-CI]: https://github.com/zio/zio-prelude/workflows/CI/badge.svg
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-prelude_2.12/ "Sonatype Releases"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/dev/zio/zio-prelude_2.12/ "Sonatype Snapshots"
[Link-Discord]: https://discord.gg/2ccFBr4 "Discord"
[Stage]: https://img.shields.io/badge/Project%20Stage-Production%20Ready-brightgreen.svg
[Stage-Page]: https://github.com/zio/zio/wiki/Project-Stages
