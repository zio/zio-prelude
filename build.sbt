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
    benchmarks,
    docs
  )
  .enablePlugins(ScalaJSPlugin)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .in(file("core"))
  .settings(stdSettings("zio-prelude"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.prelude"))
  .settings(scalacOptions in (Compile, console) ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings( // 2.13 and Dotty standard library doesn't contain Parallel Scala collections
    libraryDependencies ++= {
      val spc = List("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0" % Optional)
      Seq(
        "dev.zio" %%% "zio"          % zioVersion,
        "dev.zio" %%% "zio-test"     % zioVersion,
        "dev.zio" %%% "zio-test-sbt" % zioVersion
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

lazy val coreJS = core.js
  .settings(jsSettings)

lazy val coreJVM = core.jvm
  .settings(dottySettings)

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
