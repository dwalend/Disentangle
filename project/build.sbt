addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.3")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.2.1")

//this looked nice, but didnt quite work with sonatype. I wound up just using sbt-sonatype and tagging by hand todo remove or revisit
addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.5")

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.1.8")