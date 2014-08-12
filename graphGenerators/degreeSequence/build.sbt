name := "degSeqGraphGenerator"

version := "0.1"

scalaVersion := "2.10.3"

libraryDependencies  ++= Seq(
            // other dependencies here
            "org.scalanlp" % "breeze_2.10" % "0.7-SNAPSHOT",
            // native libraries are not included by default. add this if you want them (as of 0.7-SNAPSHOT)
            // native libraries greatly improve performance, but increase jar sizes.
            "org.scalanlp" % "breeze-natives_2.10" % "0.7-SNAPSHOT"
)

resolvers ++= Seq(
            // other resolvers here
            // if you want to use snapshot builds (currently 0.7-SNAPSHOT), use this.
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

//snapshots:
libraryDependencies  ++= Seq(
            "org.scalanlp" %% "breeze" % "0.7-SNAPSHOT"
)

// Scala 2.9.2 is still supported for 0.2.1, but is dropped afterwards.
// Don't use an earlier version of 2.10, you will probably get weird compiler crashes.
	
