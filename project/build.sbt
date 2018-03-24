addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.1")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.0")

//this looked nice, but didnt quite work with sonatype. I wound up just using sbt-sonatype and tagging by hand todo remove or revisit
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.7")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.1")