// see https://github.com/sbt/sbt-assembly
//import AssemblyKeys._ // put this at the top of the file

organization := "Hackday DevelopHer 2013"


name := "Google Analytics Application"

version := "1.0"

scalaVersion := "2.10.1"

scalaVersion in update := "2.10.1"

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked")

scalacOptions ++= Seq("-deprecation", "-unchecked", "-encoding", "utf8")

logLevel := Level.Error

resolvers ++= Seq(
  "Typesafe Releases"        at "http://repo.typesafe.com/typesafe/releases",
  "Scala-Tools Snapshots"    at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Scala Tools Releases"     at "https://oss.sonatype.org/content/groups/scala-tools/",
  "Sonatype Nexus Releases"  at "https://oss.sonatype.org/content/repositories/releases",
  "Sonatype Legacy Releases" at "https://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "com.github.scala-incubator.io" %% "scala-io-core"      % "0.4.2" withSources,
  "com.github.scala-incubator.io" %% "scala-io-file"      % "0.4.2" withSources,
  "javax.mail"                    %  "mail"               % "latest.integration" withSources,
  "org.scala-tools.time"          %  "time_2.9.1"         % "0.5",
  "org.scala-lang"                %  "scala-swing"        % "2.10.1" withSources,
  "org.scalatest"                 %% "scalatest"          % "latest.milestone"    % "test" withSources,
  "com.typesafe"                  %  "config"             % "1.0.1"                       withSources,
  "net.liftweb"                   %% "lift-webkit"        % "2.5-RC1"             % "compile",
  "net.liftmodules"               %% "lift-jquery-module" % "2.5-RC1-2.2",
  "org.eclipse.jetty"             %  "jetty-webapp"       % "8.1.7.v20120910"     % "container,test",
  "org.eclipse.jetty.orbit"       %  "javax.servlet"      % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),
  "ch.qos.logback"                %  "logback-classic"    % "1.0.6",
  "com.mongodb.casbah"            %  "casbah_2.9.0"       % "2.2.0-SNAPSHOT",
  "com.google.apis"               %  "google-api-services-analytics"    % "v3-rev68-1.17.0-rc",
  "com.google.http-client"               %  "google-http-client-jackson2"      % "1.17.0-rc",
  "com.google.oauth-client"       %  "google-oauth-client-jetty"        % "1.17.0-rc"
)

// add all the settings in assemblySettings into each SBT project
//seq(assemblySettings: _*)

seq(com.github.siasia.WebPlugin.webSettings :_*)
