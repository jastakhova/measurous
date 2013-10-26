//addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.1")

//resolvers += Resolver.url("sbt-plugin-releases",
  //new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)
addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.2.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.8")

libraryDependencies <+= sbtVersion(v => v match {
  case "0.11.0" => "com.github.siasia" %% "xsbt-web-plugin" % "0.11.0-0.2.8"
  case "0.11.1" => "com.github.siasia" %% "xsbt-web-plugin" % "0.11.1-0.2.10"
  case "0.11.2" => "com.github.siasia" %% "xsbt-web-plugin" % "0.11.2-0.2.10"
  case "0.11.3" => "com.github.siasia" %% "xsbt-web-plugin" % "0.11.3-0.2.11.1"
  case "0.12.0" => "com.github.siasia" %% "xsbt-web-plugin" % "0.12.0-0.2.11.1"
  case "0.12.1" => "com.github.siasia" %% "xsbt-web-plugin" % "0.12.0-0.2.11.1"
})