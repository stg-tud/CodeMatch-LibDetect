scalaVersion := "2.11.8"

name := "codematch"
       
resolvers += "Sonatype OSS Snapshots" at
"https://oss.sonatype.org/content/repositories/snapshots"
           
       
libraryDependencies ++= Seq(
                    "de.opal-project" %
"abstract-interpretation-framework_2.11" % "0.9.0-SNAPSHOT" withSources() withJavadoc(),
                    "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
                    )
