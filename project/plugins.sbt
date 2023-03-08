val zioSbtVersion = "0.3.10+67-a7be63df-SNAPSHOT"

addSbtPlugin("dev.zio" % "zio-sbt-ecosystem" % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-website"   % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-ci"        % zioSbtVersion)

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.1")

resolvers ++= Resolver.sonatypeOssRepos("public")

// In order to support Scala 2.11, we need to stick with sbt-scalajs 1.12.0
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.12.0")
dependencyOverrides += Defaults.sbtPluginExtra(
  "org.scala-js" % "sbt-scalajs" % "1.12.0",
  sbtBinaryVersion.value,
  scalaBinaryVersion.value
)
