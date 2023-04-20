import BuildHelper._

inThisBuild(
  List(
    organization := "dev.zio",
    homepage     := Some(url("https://zio.dev/zio-prelude/")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers   := List(
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
addCommandAlias(
  "check",
  "; scalafmtSbtCheck; scalafmtCheckAll; Test/compile; compile:scalafix --check; test:scalafix --check"
)

addCommandAlias(
  "testJVM",
  ";coreTestsJVM/test;experimentalTestsJVM/test;scalaParallelCollections/test"
)
addCommandAlias(
  "testJS",
  ";coreTestsJS/test;experimentalTestsJS/test"
)
addCommandAlias(
  "testNative",
  ";coreTestsNative/test;experimentalTestsNative/test" // `test` currently executes only compilation, see `nativeSettings` in `BuildHelper`
)

val zioVersion = "2.0.10"

val projectsCommon = List(
  core,
  coreTests,
  examples,
  experimental,
  experimentalLaws,
  experimentalTests,
  laws,
  macros
)

val projectsJvmOnly = List[ProjectReference](
  benchmarks,
  docs,
  scalaParallelCollections
)

lazy val rootJVM = project
  .in(file("target/rootJVM"))
  .settings(publish / skip := true)
  .aggregate(projectsCommon.map(_.jvm: ProjectReference): _*)
  .aggregate(projectsJvmOnly: _*)

lazy val rootJS = project
  .in(file("target/rootJS"))
  .settings(publish / skip := true)
  .aggregate(projectsCommon.map(_.js: ProjectReference): _*)

lazy val rootNative = project
  .in(file("target/rootNative"))
  .settings(publish / skip := true)
  .aggregate(projectsCommon.map(_.native: ProjectReference): _*)

lazy val root211 = project
  .in(file("target/root211"))
  .settings(publish / skip := true)
  .aggregate(projectsCommon.flatMap(p => List[ProjectReference](p.jvm, p.js, p.native)): _*)
  .aggregate(scalaParallelCollections)

lazy val root212 = project
  .in(file("target/root212"))
  .settings(publish / skip := true)
  .aggregate(projectsCommon.flatMap(p => List[ProjectReference](p.jvm, p.js, p.native)): _*)
  .aggregate(benchmarks, scalaParallelCollections)

lazy val root213 = project
  .in(file("target/root213"))
  .settings(publish / skip := true)
  .aggregate(projectsCommon.flatMap(p => List[ProjectReference](p.jvm, p.js, p.native)): _*)
  .aggregate(projectsJvmOnly: _*)

lazy val root3 = project
  .in(file("target/root3"))
  .settings(publish / skip := true)
  .aggregate(root212)

lazy val root = project
  .in(file("."))
  .settings(publish / skip := true)
  .aggregate(root213)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core"))
  .settings(stdSettings("zio-prelude"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(buildInfoSettings("zio.prelude"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"         % zioVersion,
      "dev.zio" %%% "zio-streams" % zioVersion
    )
  )
  .settings(dottySettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(macros)

lazy val coreTests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core-tests"))
  .settings(stdSettings("zio-prelude-tests"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(buildInfoSettings("zio.prelude.tests"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .jvmSettings(scalaReflectTestSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(laws)
  .settings(publish / skip := true)

lazy val laws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("laws"))
  .settings(stdSettings("zio-laws-laws"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(buildInfoSettings("zio.prelude.laws"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(libraryDependencies += "dev.zio" %%% "zio-test" % zioVersion)
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .jvmSettings(scalaReflectTestSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(core)

lazy val macros = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("macros"))
  .settings(stdSettings("zio-prelude-macros"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(buildInfoSettings("zio.prelude.macros"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(dottySettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val experimental = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("experimental"))
  .dependsOn(core)
  .settings(stdSettings("zio-prelude-experimental"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude.experimental"))
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .jvmSettings(scalaReflectTestSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val experimentalLaws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("experimental-laws"))
  .dependsOn(experimental, laws)
  .settings(stdSettings("zio-prelude-experimental-laws"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude.experimental.laws"))
  .settings(libraryDependencies += "dev.zio" %%% "zio-test" % zioVersion)
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .jvmSettings(scalaReflectTestSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val experimentalTests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("experimental-tests"))
  .dependsOn(experimentalLaws)
  .settings(stdSettings("zio-prelude-experimental-tests"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude.experimental.tests"))
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .jvmSettings(scalaReflectTestSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val scalaParallelCollections = project
  .in(file("scala-parallel-collections"))
  .dependsOn(core.jvm, coreTests.jvm % "test->test")
  .settings(stdSettings("zio-prelude-scala-parallel-collections"))
  .settings(buildInfoSettings("zio.prelude.scalaparallelcollections"))
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .settings(dottySettings)
  .settings(
    libraryDependencies ++= {
      scalaVersion.value match {
        // Only 2.11 and 2.12 standard library contains Parallel Scala collections
        case BuildHelper.Scala211 | BuildHelper.Scala212 =>
          List()
        case _                                           =>
          List("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4")
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
    crossScalaVersions --= List(BuildHelper.Scala211),
    publish / skip := true,
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"   % "2.9.0",
      "org.typelevel" %% "cats-effect" % "3.4.9"
    )
  )
  .dependsOn(core.jvm)
  .enablePlugins(JmhPlugin)

lazy val docs = project
  .in(file("zio-prelude-docs"))
  .settings(stdSettings("zio-prelude-docs"))
  .settings(
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    scalaVersion                               := Scala213,
    crossScalaVersions -= Scala211,
    projectName                                := "ZIO Prelude",
    mainModuleName                             := (core.jvm / moduleName).value,
    projectStage                               := ProjectStage.ProductionReady,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      core.jvm,
      experimental.jvm,
      experimentalLaws.jvm,
      laws.jvm,
      scalaParallelCollections
    ),
    docsPublishBranch                          := "series/2.x",
    checkArtifactBuildProcessWorkflowStep      := None
  )
  .settings(macroDefinitionSettings)
  .dependsOn(core.jvm, experimental.jvm, experimentalLaws.jvm, laws.jvm, scalaParallelCollections)
  .enablePlugins(WebsitePlugin)

lazy val examples =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("examples"))
    .dependsOn(core)
    .settings(stdSettings("zio-prelude-examples"))
    .settings(crossProjectSettings)
    .settings(macroExpansionSettings)
    .settings(publish / skip := true)
