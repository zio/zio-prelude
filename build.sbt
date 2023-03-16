import zio.sbt.ZioSbtCiPlugin.{CacheDependencies, Checkout, SetSwapSpace, SetupJava, SetupLibuv, SetupNodeJs}
import zio.sbt.githubactions.Step.{SingleStep, StepSequence}
import zio.sbt.githubactions.{Condition, Job, Strategy}

enablePlugins(ZioSbtCiPlugin)

crossScalaVersions := Seq(scala213.value)

inThisBuild(
  List(
    name                      := "ZIO Prelude",
    ciEnabledBranches         := Seq("series/2.x"),
    ciSwapSizeGB              := 7,
    ciPullRequestApprovalJobs := Seq("lint", "compile", "publishLocal", "test", "testJvms", "testPlatforms"),
    ciBuildJobs               := Seq(
      Job(
        id = "compile",
        name = "Compile",
        runsOn = "ubuntu-20.04",
        timeoutMinutes = 60,
        strategy = Some(
          Strategy(
            matrix = Map(
              "java"     -> List("17"),
              "platform" -> List("JVM", "JS", "Native")
            )
          )
        ),
        steps = Seq(
          Checkout.value,
          SetupJava("${{ matrix.java }}"),
          CacheDependencies,
          SetupLibuv,
          SetSwapSpace.value,
          SingleStep(
            name = s"Check all code compiles",
            run = Some("free --si -tmws 10 & ./sbt +root${{ matrix.platform }}/Test/compile")
          )
        )
      ),
      Job(
        id = "publishLocal",
        name = "Publish Local",
        runsOn = "ubuntu-20.04",
        timeoutMinutes = 60,
        steps = Seq(
          Checkout.value,
          SetupJava("8"),
          CacheDependencies,
          SetupLibuv,
          SetSwapSpace.value,
          SingleStep(
            name = "Check that building packages works",
            run = Some("./sbt +publishLocal")
          )
        )
      )
    ),
    ciTestJobs                := Seq(
      Job(
        id = "test",
        name = "Test",
        runsOn = "ubuntu-20.04",
        timeoutMinutes = 60,
        strategy = Some(
          Strategy(
            failFast = false,
            matrix = Map(
              "scala"    -> List("2.11.*", "2.12.*", "2.13.*", "3.*"),
              "java"     -> List("17"),
              "platform" -> List("JVM")
            )
          )
        ),
        steps = Seq(
          Checkout.value,
          SetupJava("${{ matrix.java }}"),
          CacheDependencies,
          SetupLibuv,
          SetSwapSpace.value
        ) ++ Seq("2.11", "2.12", "2.13", "3").map { sbv =>
          SingleStep(
            name = s"tests $sbv",
            condition = Some(Condition.Expression(s"startsWith(matrix.scala, '$sbv.')")),
            run = Some("free --si -tmws 10 & ./sbt ++${{ matrix.scala }} test${{ matrix.platform }}")
          )
        }
      ),
      Job(
        id = "testJvms",
        name = "Test JVMs",
        runsOn = "ubuntu-20.04",
        timeoutMinutes = 60,
        strategy = Some(
          Strategy(
            failFast = false,
            matrix = Map(
              "java"     -> List("11", "17"),
              "platform" -> List("JVM")
            )
          )
        ),
        steps = Seq(
          Checkout.value,
          SetupJava("${{ matrix.java }}"),
          CacheDependencies,
          SetupLibuv,
          SetSwapSpace.value,
          SingleStep(
            name = "Test on different JVM versions",
            run = Some("./sbt test${{ matrix.platform }}")
          )
        )
      ),
      Job(
        id = "testPlatforms",
        name = "Test Platforms",
        runsOn = "ubuntu-20.04",
        timeoutMinutes = 60,
        strategy = Some(
          Strategy(
            failFast = false,
            matrix = Map(
              "java"     -> List("17"),
              "platform" -> List("JVM", "Native")
            )
          )
        ),
        steps = Seq(
          Checkout.value,
          SetupJava("${{ matrix.java }}"),
          CacheDependencies,
          SetupLibuv,
          SetSwapSpace.value,
          SingleStep(
            name = "Test on different Scala target platforms",
            run = Some("./sbt test${{ matrix.platform }}")
          )
        )
      )
    ),
    developers                := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    )
  )
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
  .settings(crossScalaVersions := Seq(scala213.value))
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
  .settings(
    stdSettings(name = "zio-prelude", packageName = Some("zio.prelude"), enableCrossProject = true),
    enableZIO(enableStreaming = true),
    macroDefinitionSettings,
    Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) },
    scalacOptions ~= { _.filterNot(Set("-noindent")) },
    Compile / doc / scalacOptions ++= optionsOnExcept("3")(
      "-no-link-warnings"
    ).value // Suppresses problems with Scaladoc's linking
  )
  .jsSettings(jsSettings, scalajs)
  .nativeSettings(nativeSettings)
  .dependsOn(macros)

lazy val coreTests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core-tests"))
  .settings(
    stdSettings(
      name = "zio-prelude-tests",
      packageName = Some("zio.prelude.tests"),
      enableCrossProject = true
    ),
    enableZIO(),
    macroDefinitionSettings,
    Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) },
    publish / skip := true
  )
  .jvmSettings(scalaReflectTestSettings)
  .jsSettings(jsSettings, scalajs)
  .nativeSettings(nativeSettings)
  .dependsOn(laws)

lazy val laws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("laws"))
  .settings(
    stdSettings(
      name = "zio-laws-laws",
      packageName = Some("zio.prelude.laws"),
      enableCrossProject = true
    ),
    enableZIO(),
    macroDefinitionSettings,
    libraryDependencies += "dev.zio" %%% "zio-test" % zioVersion.value,
    Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) }
  )
  .jvmSettings(scalaReflectTestSettings)
  .jsSettings(jsSettings, scalajs)
  .nativeSettings(nativeSettings)
  .dependsOn(core)

lazy val macros = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("macros"))
  .settings(
    stdSettings(
      name = "zio-prelude-macros",
      packageName = Some("zio.prelude.macros"),
      enableCrossProject = true
    ),
    macroDefinitionSettings,
    Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) }
  )
  .jsSettings(jsSettings, scalajs)
  .nativeSettings(nativeSettings)

lazy val experimental = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("experimental"))
  .dependsOn(core)
  .settings(
    stdSettings(
      name = "zio-prelude-experimental",
      packageName = Some("zio.prelude.experimental"),
      enableCrossProject = true
    ),
    enableZIO()
  )
  .jvmSettings(scalaReflectTestSettings)
  .jsSettings(jsSettings, scalajs)
  .nativeSettings(nativeSettings)

lazy val experimentalLaws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("experimental-laws"))
  .dependsOn(experimental, laws)
  .settings(
    stdSettings(
      name = "zio-prelude-experimental-laws",
      packageName = Some("zio.prelude.experimental.laws"),
      enableCrossProject = true
    )
  )
  .jvmSettings(scalaReflectTestSettings)
  .jsSettings(jsSettings, scalajs)
  .nativeSettings(nativeSettings)

lazy val experimentalTests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("experimental-tests"))
  .dependsOn(experimentalLaws)
  .settings(
    stdSettings(
      name = "zio-prelude-experimental-tests",
      packageName = Some("zio.prelude.experimental.tests"),
      enableCrossProject = true
    ),
    enableZIO()
  )
  .jvmSettings(scalaReflectTestSettings)
  .jsSettings(jsSettings, scalajs)
  .nativeSettings(nativeSettings)

lazy val scalaParallelCollections = project
  .in(file("scala-parallel-collections"))
  .dependsOn(core.jvm, coreTests.jvm % "test->test")
  .settings(
    stdSettings(
      name = "zio-prelude-scala-parallel-collections",
      packageName = Some("zio.prelude.scalaparallelcollections")
    ),
    enableZIO(),
    scalaReflectTestSettings,
    addDependenciesOnOrElse("2.11", "2.12")()(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
    )
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(
    stdSettings("zio-prelude-benchmarks"),
    publish / skip := true,
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    crossScalaVersions -= scala211.value,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"   % "2.9.0",
      "org.typelevel" %% "cats-effect" % "3.4.7"
    )
  )
  .dependsOn(core.jvm)
  .enablePlugins(JmhPlugin)

lazy val docs = project
  .in(file("zio-prelude-docs"))
  .settings(
    stdSettings("zio-prelude-docs"),
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    scalaVersion                               := scala213.value,
    crossScalaVersions                         := Seq(scala212.value, scala213.value, scala3.value),
    projectName                                := (ThisBuild / name).value,
    mainModuleName                             := (core.jvm / moduleName).value,
    projectStage                               := ProjectStage.ProductionReady,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      core.jvm,
      experimental.jvm,
      experimentalLaws.jvm,
      laws.jvm,
      scalaParallelCollections
    ),
    macroDefinitionSettings
  )
  .dependsOn(core.jvm, experimental.jvm, experimentalLaws.jvm, laws.jvm, scalaParallelCollections)
  .enablePlugins(WebsitePlugin)

lazy val examples =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("examples"))
    .dependsOn(core)
    .settings(
      stdSettings(name = "zio-prelude-examples", enableCrossProject = true),
      macroExpansionSettings,
      publish / skip := true
    )

lazy val scalajs: Seq[Setting[_]] = addOptionsOn("3")("-scalajs")
