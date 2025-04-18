import org.scalajs.linker.interface.ModuleSplitStyle
import org.typelevel.sbt.gha.JobEnvironment
import org.typelevel.sbt.gha.PermissionValue
import org.typelevel.sbt.gha.Permissions

// 3.7.0-RC1 isn't ready: https://github.com/scala/scala3/issues/22794
ThisBuild / scalaVersion := "3.6.4"
ThisBuild / scalacOptions ++= Seq(
  "-no-indent",
  "-deprecation",
  "-Wunused:all",
  "-Xkind-projector",
  "-Wvalue-discard",
)

ThisBuild / githubWorkflowPermissions := Some {
  // https://github.com/typelevel/sbt-typelevel/pull/794
  Permissions.Specify(
    pages = PermissionValue.Write,
    idToken = PermissionValue.Write,
    actions = PermissionValue.None,
    checks = PermissionValue.None,
    contents = PermissionValue.Read,
    deployments = PermissionValue.None,
    issues = PermissionValue.None,
    packages = PermissionValue.None,
    pullRequests = PermissionValue.None,
    repositoryProjects = PermissionValue.None,
    securityEvents = PermissionValue.None,
    statuses = PermissionValue.None,
  )
}

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Use(
    UseRef.Public("actions", "setup-node", "v4"),
    params = Map(
      "node-version" -> "20",
      "cache" -> "yarn",
      "cache-dependency-path" -> "web/yarn.lock",
    ),
  ),
  WorkflowStep.Run(List("yarn"), workingDirectory = Some("web")),
  WorkflowStep.Run(
    List("yarn build"),
    workingDirectory = Some("web"),
  ),
  WorkflowStep.Use(
    UseRef.Public("actions", "upload-pages-artifact", "v3"),
    params = Map("path" -> "web/dist"),
  ),
  WorkflowStep.Use(
    UseRef.Public("actions", "deploy-pages", "v4")
  ),
)

ThisBuild / githubWorkflowGeneratedCI ~= {
  _.map {
    case job if job.id == "publish" =>
      job.withEnvironment(
        Some(
          JobEnvironment(
            "github-pages",
            // https://github.com/typelevel/sbt-typelevel/issues/802
            Some(new URL("https://kubukoz.github.io/stretch-the-kin")),
          )
        )
      )
    case job => job
  }
}

val web = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("com.kubukoz.stretchthekin")))
    },
    libraryDependencies ++= Seq(
      "com.armanbilge" %%% "calico" % "0.2.3",
      "org.typelevel" %%% "kittens" % "3.4.0",
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "io.circe" %%% "circe-core" % "0.14.10",
    ),
  )

val root = project
  .in(file("."))
  .aggregate(web)
