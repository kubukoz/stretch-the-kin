import org.scalajs.linker.interface.ModuleSplitStyle
import org.typelevel.sbt.gha.JobEnvironment
import org.typelevel.sbt.gha.PermissionValue
import org.typelevel.sbt.gha.Permissions

ThisBuild / scalaVersion := "3.7.1"
ThisBuild / scalacOptions ++= Seq(
  "-no-indent",
  "-deprecation",
  "-Wunused:all",
  "-Xkind-projector",
  "-Wvalue-discard",
)

ThisBuild / githubWorkflowPermissions := Some {
  Permissions
    .Specify
    .defaultRestrictive
    .withPages(PermissionValue.Write)
    .withIdToken(PermissionValue.Write)
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

ThisBuild / mergifyStewardConfig ~= (_.map(_.withMergeMinors(true)))

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
      "org.typelevel" %%% "kittens" % "3.5.0",
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "io.circe" %%% "circe-core" % "0.14.13",
    ),
  )

val root = project
  .in(file("."))
  .aggregate(web)
