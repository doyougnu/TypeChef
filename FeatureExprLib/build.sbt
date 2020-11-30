libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.core" % "2.3.5"

libraryDependencies += "de.fosd.typechef" % "javabdd_repackaged" % "1.0b2"

libraryDependencies ++= Seq(
    "com.typesafe.slick" %% "slick" % "3.3.2",
    "org.slf4j" % "slf4j-nop" % "1.6.4",
    "com.typesafe.slick" %% "slick-hikaricp" % "3.3.2"
)