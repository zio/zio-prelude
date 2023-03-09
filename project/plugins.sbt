val zioSbtVersion = "0.3.10+73-f55ad21c-SNAPSHOT"

addSbtPlugin("dev.zio" % "zio-sbt-ecosystem" % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-website"   % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-ci"        % zioSbtVersion)

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.1")

resolvers ++= Resolver.sonatypeOssRepos("public")

// In order to support Scala 2.11, we need to stick with sbt-scalajs 1.12.0
dependencyOverrides += Defaults.sbtPluginExtra(
  "org.scala-js" % "sbt-scalajs" % "1.12.0",
  sbtBinaryVersion.value,
  scalaBinaryVersion.value
)

// In order to support Scala 2.11, we need to stick with sbt-scala-native 0.4.10
dependencyOverrides += Defaults.sbtPluginExtra(
  "org.scala-native" % "sbt-scala-native" % "0.4.9",
  sbtBinaryVersion.value,
  scalaBinaryVersion.value
)
