import com.softwaremill.SbtSoftwareMillCommon.commonSmlBuildSettings
import com.softwaremill.Publish.ossPublishSettings
import sbt.Reference.display
import sbt.internal.ProjectMatrix

val scala2_12 = "2.12.17"
val scala2_13 = "2.13.9"
val scala3 = "3.2.0"

val scalaJVMVersions = List(scala2_12, scala2_13, scala3)
val scalaJSVersions = List(scala2_12, scala2_13, scala3)
val scalaNativeVersions = List(scala2_12, scala2_13, scala3)

val circeVersion = "0.14.1"
val circeYamlVersion = "0.14.1"
val scalaTestVersion = "3.2.14"

excludeLintKeys in Global ++= Set(ideSkipProject)

val commonSettings = commonSmlBuildSettings ++ ossPublishSettings ++ Seq(
  organization := "com.softwaremill.sttp.apispec",
  mimaPreviousArtifacts := Set.empty,
  versionScheme := Some("semver-spec")
)

val commonJvmSettings = commonSettings ++ Seq(
  ideSkipProject := (scalaVersion.value != scala2_13),
  libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % scalaTestVersion % Test),
  mimaPreviousArtifacts := previousStableVersion.value.map(organization.value %% moduleName.value % _).toSet,
  mimaReportBinaryIssues := { if ((publish / skip).value) {} else mimaReportBinaryIssues.value }
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
  apispecModel.projectRefs ++
    openapiModel.projectRefs ++
    openapiCirce.projectRefs ++
    openapiCirceYaml.projectRefs ++
    asyncapiModel.projectRefs ++
    asyncapiCirce.projectRefs ++
    asyncapiCirceYaml.projectRefs

lazy val projectAggregates: Seq[ProjectReference] = if (sys.env.isDefinedAt("STTP_NATIVE")) {
  println("[info] STTP_NATIVE defined, including native in the aggregate projects")
  allProjectAggregates
} else {
  println("[info] STTP_NATIVE *not* defined, *not* including native in the aggregate projects")
  allProjectAggregates.filterNot(pr => display(pr.project).contains("Native"))
}

val compileAndTest = "compile->compile;test->test"

lazy val rootProject = (project in file("."))
  .settings(commonSettings: _*)
  .settings(publish / skip := true, name := "sttp-apispec", scalaVersion := scala2_13)
  .aggregate(projectAggregates: _*)

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
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion
    ),
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
  .dependsOn(openapiModel)

lazy val openapiCirceYaml: ProjectMatrix = (projectMatrix in file("openapi-circe-yaml"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq("io.circe" %% "circe-yaml" % circeYamlVersion),
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
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion
    ),
    name := "asyncapi-circe"
  )
  .jvmPlatform(
    scalaVersions = scalaJVMVersions,
    settings = commonJvmSettings
  )
  .dependsOn(asyncapiModel)

lazy val asyncapiCirceYaml: ProjectMatrix = (projectMatrix in file("asyncapi-circe-yaml"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq("io.circe" %% "circe-yaml" % circeYamlVersion),
    name := "asyncapi-circe-yaml"
  )
  .jvmPlatform(
    scalaVersions = scalaJVMVersions,
    settings = commonJvmSettings
  )
  .dependsOn(asyncapiCirce)
