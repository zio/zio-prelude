val zioSbtVersion = "0.3.10+57-042d361d-SNAPSHOT"

addSbtPlugin("dev.zio" % "zio-sbt-ecosystem" % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-website"   % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-ci"        % zioSbtVersion)

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.1")

resolvers ++= Resolver.sonatypeOssRepos("public")
