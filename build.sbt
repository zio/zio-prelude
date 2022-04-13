import BuildHelper._

inThisBuild(
  List(
    organization := "dev.zio",
    homepage     := Some(url("https://zio.github.io/zio-prelude/")),
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
  ";lawsNative/test;experimentalLawsNative/test" // `test` currently executes only compilation, see `nativeSettings` in `BuildHelper`
)

val zioVersion = "1.0.14"

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
    coreTestsJS,
    coreTestsJVM,
    docs,
    examplesJVM,
    experimentalJS,
    experimentalJVM,
    experimentalNative,
    experimentalLawsJS,
    experimentalLawsJVM,
    experimentalLawsNative,
    experimentalTestsJS,
    experimentalTestsJVM,
    lawsJS,
    lawsJVM,
    lawsNative,
    macrosJS,
    macrosJVM,
    macrosNative,
    scalaParallelCollections
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
      "dev.zio" %%% "zio"         % zioVersion,
      "dev.zio" %%% "zio-streams" % zioVersion
    )
  )
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(macros)

lazy val coreJS = core.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val coreJVM = core.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaReflectTestSettings)

lazy val coreNative = core.native
  .settings(nativeSettings)

lazy val coreTests = crossProject(JSPlatform, JVMPlatform)
  .in(file("core-tests"))
  .settings(stdSettings("zio-prelude-tests"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(buildInfoSettings("zio.prelude.tests"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(laws)
  .settings(publish / skip := true)

lazy val coreTestsJS = coreTests.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val coreTestsJVM = coreTests.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaReflectTestSettings)

lazy val laws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("laws"))
  .settings(stdSettings("zio-laws-laws"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(buildInfoSettings("zio.prelude.laws"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(libraryDependencies += "dev.zio" %%% "zio-test" % zioVersion)
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(core)

lazy val lawsJS = laws.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val lawsJVM = laws.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaReflectTestSettings)

lazy val lawsNative = laws.native
  .settings(nativeSettings)

lazy val macros = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("macros"))
  .settings(stdSettings("zio-prelude-macros"))
  .settings(crossProjectSettings)
  .settings(macroDefinitionSettings)
  .settings(buildInfoSettings("zio.prelude.macros"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .enablePlugins(BuildInfoPlugin)

lazy val macrosJS = macros.js
  .settings(jsSettings)
  .settings(dottySettings)

lazy val macrosJVM = macros.jvm
  .settings(dottySettings)

lazy val macrosNative = macros.native
  .settings(nativeSettings)

lazy val experimental = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("experimental"))
  .dependsOn(core)
  .settings(stdSettings("zio-prelude-experimental"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude.experimental"))
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .enablePlugins(BuildInfoPlugin)

lazy val experimentalJS = experimental.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val experimentalJVM = experimental.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaReflectTestSettings)

lazy val experimentalNative = experimental.native
  .settings(nativeSettings)

lazy val experimentalLaws = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("experimental-laws"))
  .dependsOn(experimental, laws)
  .settings(stdSettings("zio-prelude-experimental-laws"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude.experimental.laws"))
  .settings(libraryDependencies += "dev.zio" %%% "zio-test" % zioVersion)
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .enablePlugins(BuildInfoPlugin)

lazy val experimentalLawsJS = experimentalLaws.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val experimentalLawsJVM = experimentalLaws.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaReflectTestSettings)

lazy val experimentalLawsNative = experimentalLaws.native
  .settings(nativeSettings)

lazy val experimentalTests = crossProject(JSPlatform, JVMPlatform)
  .in(file("experimental-tests"))
  .dependsOn(experimentalLaws)
  .settings(stdSettings("zio-prelude-experimental-tests"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude.experimental.tests"))
  .settings(testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")))
  .enablePlugins(BuildInfoPlugin)

lazy val experimentalTestsJS = experimentalTests.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)

lazy val experimentalTestsJVM = experimentalTests.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaReflectTestSettings)

lazy val scalaParallelCollections = project
  .in(file("scala-parallel-collections"))
  .dependsOn(coreJVM % "compile->compile;test->test", coreTestsJVM % "test->test")
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
      "org.typelevel" %% "cats-core" % "2.7.0"
    )
  )
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin)

lazy val docs = project
  .in(file("zio-prelude-docs"))
  .settings(
    publish / skip                             := true,
    moduleName                                 := "zio-prelude-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(coreJVM, experimentalJVM),
    ScalaUnidoc / unidoc / target              := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite                       := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages                   := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .settings(macroDefinitionSettings)
  .dependsOn(coreJVM, experimentalJVM, lawsJVM)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val examples =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .in(file("examples"))
    .dependsOn(core)
    .settings(stdSettings("zio-prelude-examples"))
    .settings(crossProjectSettings)
    .settings(macroExpansionSettings)
    .settings(publish / skip := true)

lazy val examplesJVM = examples.jvm
  .settings(dottySettings)
