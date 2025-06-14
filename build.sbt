import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings
import com.softwaremill.Publish.ossPublishSettings
import sbt.Reference.display
import sbt.internal.ProjectMatrix

val scala2_12 = "2.12.20"
val scala2_13 = "2.13.16"
val scala3 = "3.3.5"

val scalaJVMVersions = List(scala2_12, scala2_13, scala3)
val scalaJSVersions = List(scala2_12, scala2_13, scala3)
val scalaNativeVersions = List(scala2_13, scala3)

val circeVersion = "0.14.12"
val circeYamlVersionCompat212 = "0.15.2"
val circeYamlVersionLatest = "0.16.1"

val scalaTestVersion = "3.2.19"
val scalaCollectionCompatVersion = "2.11.0"

excludeLintKeys in Global ++= Set(ideSkipProject)

val scala3Settings = Seq(
  scalacOptions -= "-Xmax-inlines",
  scalacOptions ++= {
    if (scalaVersion.value.startsWith("3")) List("-Xmax-inlines", "64") else Nil
  }
)

val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  organization := "com.softwaremill.sttp.apispec",
  mimaPreviousArtifacts := Set.empty,
  versionScheme := Some("early-semver")
) ++ scala3Settings

val commonJvmSettings = commonSettings ++ Seq(
  ideSkipProject := (scalaVersion.value != scala2_13),
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % scalaTestVersion % Test),
  mimaPreviousArtifacts := previousStableVersion.value.map(organization.value %% moduleName.value % _).toSet,
  mimaReportBinaryIssues := {
    if ((publish / skip).value) {} else mimaReportBinaryIssues.value
  }
)

val commonJsSettings = commonSettings ++ Seq(
  ideSkipProject := true,
  Compile / scalacOptions ++= {
    if (isSnapshot.value) Seq.empty
    else
      Seq {
        val mapSourcePrefix =
          if (ScalaArtifacts.isScala3(scalaVersion.value))
            "-scalajs-mapSourceURI"
          else
            "-P:scalajs:mapSourceURI"
        val dir = project.base.toURI.toString.replaceFirst("[^/]+/?$", "")
        val url = "https://raw.githubusercontent.com/softwaremill/sttp-apispec"
        s"$mapSourcePrefix:$dir->$url/v${version.value}/"
      }
  },
  libraryDependencies ++= Seq("org.scalatest" %%% "scalatest" % scalaTestVersion % Test)
)

val commonNativeSettings = commonSettings ++ Seq(
  ideSkipProject := true,
  libraryDependencies ++= Seq("org.scalatest" %%% "scalatest" % scalaTestVersion % Test)
)

lazy val allProjectAggregates: Seq[ProjectReference] =
  circeTestUtils.projectRefs ++
    apispecModel.projectRefs ++
    jsonSchemaCirce.projectRefs ++
    openapiModel.projectRefs ++
    openapiCirce.projectRefs ++
    openapiCirceYaml.projectRefs ++
    asyncapiModel.projectRefs ++
    asyncapiCirce.projectRefs ++
    asyncapiCirceYaml.projectRefs ++
    openapiComparatorTests.projectRefs

lazy val projectAggregates: Seq[ProjectReference] = if (sys.env.isDefinedAt("STTP_NATIVE")) {
  println("[info] STTP_NATIVE defined, including native in the aggregate projects")
  allProjectAggregates
} else {
  println("[info] STTP_NATIVE *not* defined, *not* including native in the aggregate projects")
  allProjectAggregates.filterNot(pr => display(pr.project).contains("Native"))
}

val compileAndTest = "compile->compile;test->test"

lazy val rootProject = (project in file("."))
  .settings(commonSettings *)
  .settings(publish / skip := true, name := "sttp-apispec", scalaVersion := scala2_13)
  .aggregate(projectAggregates *)

lazy val circeTestUtils: ProjectMatrix = (projectMatrix in file("circe-testutils"))
  .settings(commonSettings)
  .settings(
    publish / skip := true,
    name := "circe-testutils",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion
    )
  )
  .jvmPlatform(
    scalaVersions = scalaJVMVersions,
    settings = commonJvmSettings
  )
  .jsPlatform(
    scalaVersions = scalaJSVersions,
    settings = commonJsSettings
  )
  .nativePlatform(
    scalaVersions = scalaNativeVersions,
    settings = commonNativeSettings
  )
// apispec

lazy val apispecModel: ProjectMatrix = (projectMatrix in file("apispec-model"))
  .settings(commonSettings)
  .settings(
    name := "apispec-model"
  )
  .jvmPlatform(
    scalaVersions = scalaJVMVersions,
    settings = commonJvmSettings
  )
  .jsPlatform(
    scalaVersions = scalaJSVersions,
    settings = commonJsSettings
  )
  .nativePlatform(
    scalaVersions = scalaNativeVersions,
    settings = commonNativeSettings
  )

// jsonschema
lazy val jsonSchemaCirce: ProjectMatrix = (projectMatrix in file("jsonschema-circe"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion,
      "io.circe" %%% "circe-generic" % circeVersion
    ),
    name := "jsonschema-circe"
  )
  .jvmPlatform(
    scalaVersions = scalaJVMVersions,
    settings = commonJvmSettings
  )
  .jsPlatform(
    scalaVersions = scalaJSVersions,
    settings = commonJsSettings
  )
  .nativePlatform(
    scalaVersions = scalaNativeVersions,
    settings = commonNativeSettings
  )
  .dependsOn(apispecModel, circeTestUtils % Test)

// openapi

lazy val openapiModel: ProjectMatrix = (projectMatrix in file("openapi-model"))
  .settings(commonSettings)
  .settings(
    name := "openapi-model"
  )
  .jvmPlatform(
    scalaVersions = scalaJVMVersions,
    settings = commonJvmSettings
  )
  .jsPlatform(
    scalaVersions = scalaJSVersions,
    settings = commonJsSettings
  )
  .nativePlatform(
    scalaVersions = scalaNativeVersions,
    settings = commonNativeSettings
  )
  .dependsOn(apispecModel)

lazy val openapiCirce: ProjectMatrix = (projectMatrix in file("openapi-circe"))
  .settings(commonSettings)
  .settings(
    name := "openapi-circe"
  )
  .jvmPlatform(
    scalaVersions = scalaJVMVersions,
    settings = commonJvmSettings
  )
  .jsPlatform(
    scalaVersions = scalaJSVersions,
    settings = commonJsSettings
  )
  .nativePlatform(
    scalaVersions = scalaNativeVersions,
    settings = commonNativeSettings
  )
  .dependsOn(openapiModel, jsonSchemaCirce, circeTestUtils % Test)

lazy val openapiCirceYaml: ProjectMatrix = (projectMatrix in file("openapi-circe-yaml"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 =>
        Seq("io.circe" %% "circe-yaml" % circeYamlVersionCompat212)
      case _ =>
        Seq("io.circe" %% "circe-yaml" % circeYamlVersionLatest)
    }),
    name := "openapi-circe-yaml"
  )
  .jvmPlatform(
    scalaVersions = scalaJVMVersions,
    settings = commonJvmSettings
  )
  .dependsOn(openapiCirce)

// asyncapi

lazy val asyncapiModel: ProjectMatrix = (projectMatrix in file("asyncapi-model"))
  .settings(commonSettings)
  .settings(
    name := "asyncapi-model"
  )
  .jvmPlatform(
    scalaVersions = scalaJVMVersions,
    settings = commonJvmSettings
  )
  .jsPlatform(
    scalaVersions = scalaJSVersions,
    settings = commonJsSettings
  )
  .nativePlatform(
    scalaVersions = scalaNativeVersions,
    settings = commonNativeSettings
  )
  .dependsOn(apispecModel)

lazy val asyncapiCirce: ProjectMatrix = (projectMatrix in file("asyncapi-circe"))
  .settings(commonSettings)
  .settings(
    name := "asyncapi-circe"
  )
  .jvmPlatform(
    scalaVersions = scalaJVMVersions,
    settings = commonJvmSettings
  )
  .jsPlatform(
    scalaVersions = scalaJSVersions,
    settings = commonJsSettings
  )
  .nativePlatform(
    scalaVersions = scalaNativeVersions,
    settings = commonNativeSettings
  )
  .dependsOn(asyncapiModel, jsonSchemaCirce)

lazy val asyncapiCirceYaml: ProjectMatrix = (projectMatrix in file("asyncapi-circe-yaml"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, n)) if n <= 12 =>
        Seq("io.circe" %% "circe-yaml" % circeYamlVersionCompat212)
      case _ =>
        Seq("io.circe" %% "circe-yaml" % circeYamlVersionLatest)
    }),
    name := "asyncapi-circe-yaml"
  )
  .jvmPlatform(
    scalaVersions = scalaJVMVersions,
    settings = commonJvmSettings
  )
  .dependsOn(asyncapiCirce)

lazy val openapiComparatorTests: ProjectMatrix = (projectMatrix in file("openapi-comparator-tests"))
  .settings(commonSettings)
  .settings(
    name := "openapi-comparator-tests",
    publish / skip := true
  )
  .jvmPlatform(
    scalaVersions = scalaJVMVersions,
    settings = commonJvmSettings
  )
  .jsPlatform(
    scalaVersions = scalaJSVersions,
    settings = commonJsSettings
  )
  .nativePlatform(
    scalaVersions = scalaNativeVersions,
    settings = commonNativeSettings
  )
  .dependsOn(openapiModel, openapiCirce, circeTestUtils % Test)
