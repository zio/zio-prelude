import BuildHelper._

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.github.io/zio-prelude/")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    scmInfo := Some(
      ScmInfo(url("https://github.com/zio/zio-prelude/"), "scm:git:git@github.com:zio/zio-prelude.git")
    )
  )
)

addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check")
addCommandAlias(
  "testJVM",
  ";coreJVM/test"
)
addCommandAlias(
  "testJS",
  ";coreJS/test"
)
addCommandAlias(
  "testNative",
  ";coreNative/test:compile"
)

val zioVersion = "1.0.3"

lazy val root = project
  .in(file("."))
  .settings(
    skip in publish := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    coreJVM,
    coreJS,
    coreNative,
    benchmarks,
    docs
  )
  .enablePlugins(ScalaJSPlugin)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core"))
  .settings(stdSettings("zio-prelude"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude"))
  .settings(scalacOptions in (Compile, console) ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings( // 2.13 and Dotty standard library doesn't contain Parallel Scala collections
    libraryDependencies ++= {
      val spc = List("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0" % Optional)
      Seq(
        "dev.zio" %%% "zio"      % zioVersion,
        "dev.zio" %%% "zio-test" % zioVersion
      ) ++
        (scalaVersion.value match {
          case BuildHelper.Scala213   => spc
          case BuildHelper.ScalaDotty => spc.map(_.withDottyCompat(scalaVersion.value))
          case _                      => List()
        })
    }
  )
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .enablePlugins(BuildInfoPlugin)

lazy val coreJS     = core.js
  .settings(jsSettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion)

lazy val coreJVM    = core.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion)

lazy val coreNative = core.native
  .settings(scalaVersion := Scala211)
  .settings(crossScalaVersions := Seq(scalaVersion.value))
  .settings(skip in Test := true)
  .settings(skip in doc := true)
  .settings(       // Exclude from Intellij because Scala Native projects break it - https://github.com/scala-native/scala-native/issues/1007#issuecomment-370402092
    SettingKey[Boolean]("ide-skip-project") := true
  )
  .settings(sources in (Compile, doc) := Seq.empty)
  .settings(
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    dependencyOverrides += "dev.zio" %%% "zio" % "1.0.3+68-eaa7424f-SNAPSHOT"
  )
  .disablePlugins(
    ScalafixPlugin // for some reason `ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value)` isn't enough
  )

lazy val experimental = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("experimental"))
  .dependsOn(core)
  .settings(stdSettings("zio-prelude-experimental"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude.experimental"))

lazy val experimentalJVM    = core.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion)

lazy val experimentalJS     = core.js
  .settings(jsSettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion)

lazy val experimentalNative = core.native
  .settings(scalaVersion := Scala211)
  .settings(crossScalaVersions := Seq(scalaVersion.value))
  .settings(skip in Test := true)
  .settings(skip in doc := true)
  .settings(       // Exclude from Intellij because Scala Native projects break it - https://github.com/scala-native/scala-native/issues/1007#issuecomment-370402092
    SettingKey[Boolean]("ide-skip-project") := true
  )
  .settings(sources in (Compile, doc) := Seq.empty)
  .settings(
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    dependencyOverrides += "dev.zio" %%% "zio" % "1.0.3+68-eaa7424f-SNAPSHOT"
  )

lazy val benchmarks = project.module
  .settings(
    skip.in(publish) := true,
    moduleName := "zio-prelude-benchmarks",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      ("org.typelevel" %% "cats-core" % "2.2.0") match {
        case cats if isDotty.value => cats.withDottyCompat(scalaVersion.value)
        case cats                  => cats
      }
    )
  )
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin)

lazy val docs = project
  .in(file("zio-prelude-docs"))
  .settings(
    skip.in(publish) := true,
    moduleName := "zio-prelude-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion
    ),
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(coreJVM),
    target in (ScalaUnidoc, unidoc) := (baseDirectory in LocalRootProject).value / "website" / "static" / "api",
    cleanFiles += (target in (ScalaUnidoc, unidoc)).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(unidoc in Compile).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(unidoc in Compile).value
  )
  .dependsOn(coreJVM, coreJS)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
