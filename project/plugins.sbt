val zioSbtVersion = "0.3.10+60-2af21ed8-SNAPSHOT"

addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.12.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.2.0")

addSbtPlugin("dev.zio" % "zio-sbt-ecosystem" % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-website"   % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-ci"        % zioSbtVersion)

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.1")

resolvers ++= Resolver.sonatypeOssRepos("public")
