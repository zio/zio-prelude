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
    )
  )
)

addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check")

addCommandAlias(
  "testJVM",
  ";coreJVM/test;experimentalJVM/test;scalaParallelCollections/test"
)
addCommandAlias(
  "testJS",
  ";coreJS/test;experimentalJS/test"
)
addCommandAlias(
  "testNative",
  ";coreNative/test;experimentalNative/test" // `test` currently executes only compilation, see `nativeSettings` in `BuildHelper`
)

val zioVersion = "1.0.9"

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    benchmarks,
    coreJS,
    coreJVM,
    coreNative,
    docs,
    examplesJVM,
    experimentalJS,
    experimentalJVM,
    experimentalNative,
    scalaParallelCollections,
    macros.js,
    macros.jvm,
    macros.native
  )

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core"))
  .settings(stdSettings("zio-prelude"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(buildInfoSettings("zio.prelude"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"      % zioVersion,
      "dev.zio" %%% "zio-test" % zioVersion
    )
  )
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(macros)

lazy val coreJS  = core.js
  .settings(jsSettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val coreJVM = core.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaReflectTestSettings)

lazy val coreNative = core.native
  .settings(nativeSettings)

lazy val macros = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("macros"))
  .settings(stdSettings("zio-prelude-macros"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(buildInfoSettings("zio.prelude.macros"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .enablePlugins(BuildInfoPlugin)

lazy val experimental = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("experimental"))
  .dependsOn(core)
  .settings(stdSettings("zio-prelude-experimental"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude.experimental"))
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .enablePlugins(BuildInfoPlugin)

lazy val experimentalJS  = experimental.js
  .settings(jsSettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val experimentalJVM = experimental.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaReflectTestSettings)

lazy val experimentalNative = experimental.native
  .settings(nativeSettings)

lazy val scalaParallelCollections = project
  .in(file("scala-parallel-collections"))
  .dependsOn(coreJVM % "compile->compile;test->test")
  .settings(stdSettings("zio-prelude-scala-parallel-collections"))
  .settings(buildInfoSettings("zio.prelude.scalaparallelcollections"))
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .settings(dottySettings)
  .settings(
    libraryDependencies ++= {
      scalaVersion.value match {
        case BuildHelper.Scala213 | BuildHelper.ScalaDotty =>
          // 2.13 and Dotty standard library doesn't contain Parallel Scala collections
          List("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3")
        case _                                             =>
          List()
      }
    }
  )
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaReflectTestSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(stdSettings("zio-prelude-benchmarks"))
  .settings(
    publish / skip := true,
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.4.2"
    )
  )
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin)

lazy val docs = project
  .in(file("zio-prelude-docs"))
  .settings(
    publish / skip := true,
    moduleName := "zio-prelude-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(coreJVM, experimentalJVM),
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .dependsOn(coreJVM, experimentalJVM)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val examples =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("examples"))
    .dependsOn(core)
    .settings(stdSettings("zio-prelude-examples"))
    .settings(crossProjectSettings)
    .settings(macroExpansionSettings)

lazy val examplesJVM = examples.jvm
  .settings(dottySettings)
