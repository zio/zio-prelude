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

val zioVersion = "1.0.3"
libraryDependencies ++= Seq(
  "dev.zio" %% "zio"          % zioVersion,
  "dev.zio" %% "zio-test"     % zioVersion,
  "dev.zio" %% "zio-test-sbt" % zioVersion
)

testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val root =
  (project in file("."))
    .settings(stdSettings("zio-prelude"))
    .settings(dottySettings)
    .settings(buildInfoSettings("zio.prelude"))
    .settings(scalacOptions in (Compile, console) ~= { _.filterNot(Set("-Xfatal-warnings")) })
    .settings( // 2.13 and Dotty standard library doesn't contain Parallel Scala collections
      libraryDependencies ++= {
        if (List(BuildHelper.Scala213, BuildHelper.ScalaDotty).contains(scalaVersion.value)) {
          List("org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0" intransitive ())
        } else {
          List()
        }
      }
    )
    .enablePlugins(BuildInfoPlugin)

lazy val benchmarks = project
  .in(file("zio-prelude-benchmarks"))
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
  .dependsOn(root)
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
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(root),
    target in (ScalaUnidoc, unidoc) := (baseDirectory in LocalRootProject).value / "website" / "static" / "api",
    cleanFiles += (target in (ScalaUnidoc, unidoc)).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(unidoc in Compile).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(unidoc in Compile).value
  )
  .dependsOn(root)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
