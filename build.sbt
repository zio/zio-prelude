enablePlugins(ZioSbtCiPlugin)

crossScalaVersions := Seq(scala213.value)

inThisBuild(
  List(
    name                                  := "ZIO Prelude",
    ciEnabledBranches                     := Seq("series/2.x"),
    checkArtifactBuildProcessWorkflowStep := None,
    javaPlatforms                         := Seq("11", "17"),
    ciSwapSizeGB                          := 12,
    parallelTestExecution                 := false,
    sbtBuildOptions                       := List("-J-XX:+UseG1GC", "-J-Xmx6g", "-J-Xms4g", "-J-Xss16m"),
    supportedScalaVersions                := Map(
      (benchmarks / thisProject).value.id               -> (benchmarks / crossScalaVersions).value,
      (core.js / thisProject).value.id                  -> (core.js / crossScalaVersions).value,
      (core.jvm / thisProject).value.id                 -> (core.jvm / crossScalaVersions).value,
      (core.native / thisProject).value.id              -> (core.native / crossScalaVersions).value,
      (coreTests.js / thisProject).value.id             -> (coreTests.js / crossScalaVersions).value,
      (coreTests.jvm / thisProject).value.id            -> (coreTests.jvm / crossScalaVersions).value,
      (coreTests.native / thisProject).value.id         -> (coreTests.native / crossScalaVersions).value,
      (examples.js / thisProject).value.id              -> (examples.js / crossScalaVersions).value,
      (examples.jvm / thisProject).value.id             -> (examples.jvm / crossScalaVersions).value,
      (examples.native / thisProject).value.id          -> (examples.native / crossScalaVersions).value,
      (examples.native / thisProject).value.id          -> (examples.native / crossScalaVersions).value,
      (experimental.js / thisProject).value.id          -> (experimental.js / crossScalaVersions).value,
      (experimental.jvm / thisProject).value.id         -> (experimental.jvm / crossScalaVersions).value,
      (experimental.native / thisProject).value.id      -> (experimental.native / crossScalaVersions).value,
      (experimentalLaws.js / thisProject).value.id      -> (experimentalLaws.js / crossScalaVersions).value,
      (experimentalLaws.jvm / thisProject).value.id     -> (experimentalLaws.jvm / crossScalaVersions).value,
      (experimentalLaws.native / thisProject).value.id  -> (experimentalLaws.native / crossScalaVersions).value,
      (experimentalTests.js / thisProject).value.id     -> (experimentalTests.js / crossScalaVersions).value,
      (experimentalTests.jvm / thisProject).value.id    -> (experimentalTests.jvm / crossScalaVersions).value,
//      (experimentalTests.native / thisProject).value.id -> (experimentalTests.native / crossScalaVersions).value,
      (laws.js / thisProject).value.id                  -> (laws.js / crossScalaVersions).value,
      (laws.jvm / thisProject).value.id                 -> (laws.jvm / crossScalaVersions).value,
      (laws.native / thisProject).value.id              -> (laws.native / crossScalaVersions).value,
      (macros.js / thisProject).value.id                -> (macros.js / crossScalaVersions).value,
      (macros.jvm / thisProject).value.id               -> (macros.jvm / crossScalaVersions).value,
      (macros.native / thisProject).value.id            -> (macros.native / crossScalaVersions).value,
      (scalaParallelCollections / thisProject).value.id -> (scalaParallelCollections / crossScalaVersions).value
    ),
    developers                            := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    )
  )
)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core"))
  .settings(
    stdSettings(name = "zio-prelude", packageName = Some("zio.prelude"), enableCrossProject = true),
    enableZIO(enableStreaming = true),
    macroDefinitionSettings,
    Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) },
    scalacOptions ~= { _.filterNot(Set("-noindent")) }
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

lazy val scalajs: Seq[Setting[_]] =
  Seq(
    scalacOptions ++= {
      if (scalaVersion.value == scala3.value) {
        Seq("-scalajs")
      } else Seq.empty
    }
  )

lazy val scalaParallelCollections = project
  .in(file("scala-parallel-collections"))
  .dependsOn(core.jvm, coreTests.jvm % "test->test")
  .settings(
    stdSettings(
      name = "zio-prelude-scala-parallel-collections",
      packageName = Some("zio.prelude.scalaparallelcollections")
    ),
    enableZIO(),
    libraryDependencies ++= {
      scalaBinaryVersion.value match {
        // Only 2.11 and 2.12 standard library contains Parallel Scala collections
        case "2.11" | "2.12" =>
          List()
        case _               =>
          List("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4")
      }
    }
  )
  .settings(scalaReflectTestSettings)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(
    stdSettings("zio-prelude-benchmarks"),
    publish / skip := true,
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
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
