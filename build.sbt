import BuildHelper.*
import zio.sbt.ZioSbtCiPlugin._
import zio.sbt.githubactions.{Condition, DependencyBot, Job, Step, Strategy}

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / concurrentRestrictions += Tags.limit(NativeTags.Link, java.lang.Runtime.getRuntime.availableProcessors())

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
    ),

    // zio-sbt-ci settings: keep the generated ci.yml matching what this project's handwritten
    // workflow already tested (series/2.x only, one grouped JVM-only test job per Scala version,
    // a separate JS/Native job reusing the existing testJS/testNative aliases, a JDK 11/21 re-run,
    // and JDK 11 for the build/publish/release jobs since artifacts are published for JDK 11)
    // rather than the plugin's stock defaults.
    ciEnabledBranches       := Seq("series/2.x"),
    // Preserves the memory tuning and Node heap size the old workflow applied, and the swap space
    // that cross-building across three Scala versions needed to avoid OOM/disk pressure.
    ciJvmOptions            := Seq("-Xms6G", "-Xmx6G"),
    ciNodeOptions           := Seq("--max_old_space_size=6144"),
    ciWorkflowEnv           := {
      val flags = ("-XX:+PrintCommandLineFlags" +: ciJvmOptions.value).mkString(" ")
      Map("JDK_JAVA_OPTIONS" -> flags, "SBT_OPTS" -> flags, "NODE_OPTIONS" -> ciNodeOptions.value.mkString(" "))
    },
    ciSwapSizeGB            := 7,
    // Pin the runner image the old workflow used everywhere. The plugin's own `ubuntu-latest`
    // default now resolves to Ubuntu 24.04, whose glibc trips a heap-corruption abort in Scala
    // Native's allocator (a `malloc.c` assertion failure) that never fired under 22.04 - see the
    // "Test Platforms (Native)" failure on the CI-migration PR's own merge run.
    ciLintJobs              := ciLintJobs.value.map(onUbuntu22),
    ciUpdateReadmeJobs      := ciUpdateReadmeJobs.value.map(onUbuntu22),
    ciPostReleaseJobs       := ciPostReleaseJobs.value.map(onUbuntu22),
    ciDependencyUpdateBots  := Seq(DependencyBot.Dependabot, DependencyBot.Custom("scala-steward")),
    // The old workflow's cross-Scala `test` job only ever exercised the JVM platform; JS/Native
    // were tested separately, and only for core/experimental, via the testJS/testNative aliases
    // below (kept as the dedicated `testPlatforms` job).
    ciTargetScalaVersions   := targetScalaVersionsFor(jvmOnlyProjects: _*).value,
    ciGroupSimilarTests     := true,
    ciTargetJavaVersions    := Seq("17"),
    ciUpdateReadmeCondition := Some(Condition.Expression("github.event_name == 'push'")),
    // JDK 11/21 re-run of the JVM projects, mirroring the old `testJvms` job. JDK 17 is already
    // covered by the grouped `test` job above, so it's left out here.
    // `testPlatforms` mirrors the old job of the same name: JS/Native, default Scala only, via the
    // testJS/testNative command aliases (core/experimental only, not every module).
    ciTestJobs              := (ciTestJobs.value ++ Seq(
      Job(
        id = "testJvms",
        name = "Test JVMs",
        strategy = Some(Strategy(matrix = Map("java" -> List("11", "21")), failFast = false)),
        steps = (if (ciSwapSizeGB.value > 0) Seq(SetSwapSpace.value) else Seq.empty) ++ Seq(
          Checkout.value,
          SetupJava("${{ matrix.java }}"),
          SetupSBT,
          CacheDependencies,
          Step.SingleStep(
            name = "Test",
            run = Some("sbt --no-colors " + jvmOnlyProjects.map(_.id + "/test").mkString(" "))
          )
        )
      ),
      Job(
        id = "testPlatforms",
        name = "Test Platforms",
        strategy = Some(Strategy(matrix = Map("platform" -> List("JS", "Native")), failFast = false)),
        steps = (if (ciSwapSizeGB.value > 0) Seq(SetSwapSpace.value) else Seq.empty) ++ Seq(
          Checkout.value,
          SetupJava("17"),
          SetupSBT,
          CacheDependencies,
          Step.SingleStep(
            name = "Test on different Scala target platforms",
            run = Some("sbt --no-colors test${{ matrix.platform }}")
          )
        )
      )
    )).map(onUbuntu22),
    // The build (compile + publishLocal + website) and release jobs must run on JDK 11: published
    // artifacts target JDK 11, which is only actually exercised by compiling under it.
    ciBuildJobs             := ciBuildJobs.value.map(onJava11).map(onUbuntu22),
    ciReleaseJobs           := ciReleaseJobs.value.map(onJava11).map(onUbuntu22)
  )
)

def onJava11(job: Job): Job =
  job.copy(steps = job.steps.map {
    case s: Step.SingleStep if s.name == "Setup Scala" => SetupJava("11")
    case other                                         => other
  })

def onUbuntu22(job: Job): Job = job.copy(runsOn = "ubuntu-22.04")

addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias(
  "check",
  "; scalafmtSbtCheck; scalafmtCheckAll; Test/compile; compile:scalafix --check; test:scalafix --check"
)
// The `lint` job zio-sbt-ci generates by default runs `sbt lint`; alias it to this project's own
// formatting/scalafix check rather than pulling in zio-sbt-ecosystem for its `lint` command.
addCommandAlias("lint", "check")

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
  ";coreTestsNative/test;experimentalTestsNative/test"
)

val zioVersion = "2.1.23"

val projectsCommon = List(
  core,
  coreTests,
  examples,
  experimental,
  experimentalLaws,
  experimentalTests,
  laws,
  macros,
  magnolia,
  magnoliaTests
)

val projectsJvmOnly = List[ProjectReference](
  benchmarks,
  docs
)

// The projects the old rootJVM/root212/root213/root3 aggregates all tested: every crossProject's
// JVM variant, scalaParallelCollections' JVM variant, plus the plain JVM-only projects. Used both
// for `ciTargetScalaVersions` and for the `testJvms` job's command line below.
val jvmOnlyProjects: List[Project] = projectsCommon.map(_.jvm) ++ List(scalaParallelCollections.jvm, benchmarks, docs)

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip     := true,
    // `Nil` rather than the default `Seq(scalaVersion.value)`: `root` isn't itself cross-built,
    // and this is what lets top-level `+Test/compile`/`+publishLocal` (as run by the zio-sbt-ci
    // `build` job) cross-build over each aggregated project's own `crossScalaVersions` instead of
    // just root's.
    crossScalaVersions := Nil
  )
  .aggregate(projectsCommon.flatMap(p => List[ProjectReference](p.jvm, p.js, p.native)): _*)
  .aggregate(scalaParallelCollections.jvm, scalaParallelCollections.native)
  .aggregate(projectsJvmOnly: _*)

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

lazy val magnolia = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("magnolia"))
  .dependsOn(core)
  .settings(stdSettings("zio-prelude-magnolia"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(buildInfoSettings("zio.prelude.magnolia"))
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .settings(dottySettings)
  .settings(magnoliaSettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .jvmSettings(scalaReflectTestSettings)
  .jsSettings(jsSettings)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val magnoliaTests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("magnolia-tests"))
  .dependsOn(magnolia)
  .settings(stdSettings("zio-prelude-magnolia-tests"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude.magnolia.tests"))
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .jvmSettings(scalaReflectTestSettings)
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

lazy val scalaParallelCollections = crossProject(JVMPlatform, NativePlatform)
  .in(file("scala-parallel-collections"))
  .dependsOn(core, coreTests % "test->test")
  .settings(stdSettings("zio-prelude-scala-parallel-collections"))
  .settings(buildInfoSettings("zio.prelude.scalaparallelcollections"))
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .settings(dottySettings)
  .settings(
    libraryDependencies ++= {
      scalaVersion.value match {
        // Only 2.12 standard library contains Parallel Scala collections
        case BuildHelper.Scala212 =>
          List()
        case _                    =>
          List("org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0")
      }
    }
  )
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .jvmSettings(scalaReflectTestSettings)
  .nativeSettings(nativeSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(stdSettings("zio-prelude-benchmarks"))
  .settings(
    publish / skip := true,
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"   % "2.13.0",
      "org.typelevel" %% "cats-effect" % "3.6.4"
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
    scalaVersion                               := "2.13.18",
    crossScalaVersions                         := Seq("2.13.18"),
    projectName                                := "ZIO Prelude",
    mainModuleName                             := (core.jvm / moduleName).value,
    projectStage                               := ProjectStage.ProductionReady,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
      core.jvm,
      experimental.jvm,
      experimentalLaws.jvm,
      laws.jvm,
      scalaParallelCollections.jvm
    )
  )
  .settings(macroDefinitionSettings)
  .dependsOn(core.jvm, experimental.jvm, experimentalLaws.jvm, laws.jvm, scalaParallelCollections.jvm)
  .enablePlugins(WebsitePlugin)

lazy val examples =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("examples"))
    .dependsOn(core)
    .settings(stdSettings("zio-prelude-examples"))
    .settings(crossProjectSettings)
    .settings(macroExpansionSettings)
    .settings(publish / skip := true)
