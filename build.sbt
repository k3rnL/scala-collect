import sbtcrossproject.CrossPlugin.autoImport._

import scala.scalanative.build.{GC, LTO, Mode}


lazy val collect = crossProject(NativePlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.clickhouse" % "clickhouse-http-client" % "0.3.2-patch9",
      "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.17.2"
    )
  )
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-json" % "2.9.2",
    )
  )
  .nativeSettings(
    nativeConfig ~= {
      _.withMode(Mode.releaseFull)
        .withLTO(LTO.full)
        .withOptimize(true)
        .withGC(GC.commix)
        .withCompileOptions(Seq(
          //        "-v",
          "--target=arm64-apple-darwin21.4.0",
          "-I/Users/edaniel/CLionProjects/clickhouse-cpp-11/",
          "-I/Users/edaniel/CLionProjects/clickhouse-cpp-11/contrib",
          "-Wno-unused-command-line-argument",
          "-L/Users/edaniel/CLionProjects/clickhouse-cpp-11/cmake-build-release/clickhouse",
        ))
        .withLinkingOptions(Seq(
//                  "-v",
          //        "-static",
          "--target=arm64-apple-darwin21.4.0",
          "-Wno-unused-command-line-argument",
          "-L/Users/edaniel/CLionProjects/clickhouse-cpp-11/cmake-build-release/clickhouse",
          "-lclickhouse-cpp-lib",
        ))
    }
  )

//lazy val sharedJVM = collect.jvm
//lazy val sharedJS = collect.native