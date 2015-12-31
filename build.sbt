name := "scala-contour"

version := "1.0"

scalaVersion := "2.11.7"

//Library repositories
resolvers ++= Seq(
  Resolver.mavenLocal,
  "Scala-Tools Maven2 Repository" at "http://scala-tools.org/repo-releases",
  "GeoTools" at "http://download.osgeo.org/webdav/geotools",
  "Typesafe" at "https://repo.typesafe.com/typesafe/releases/"
)

val geotools_version = "13.2"
val akka_version = "2.4.0"

//Library Dependencies for dev
libraryDependencies ++= Seq(
  //GeoTools
  "org.geotools" % "gt-geojson" % geotools_version,

  // Akka
  "com.typesafe.akka" %% "akka-actor" % akka_version,

  //Testing
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)