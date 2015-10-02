addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.5.1")

//this looked nice, but didnt quite work with sonatype. I wound up just using sbt-sonatype and tagging by hand todo remove or revisit
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.0")