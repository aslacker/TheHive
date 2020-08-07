package org.thp.elastictool

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import play.api.Configuration
import scopt.OParser

import scala.collection.JavaConverters._

object ElasticTool extends App {
  def getVersion: String = Option(getClass.getPackage.getImplementationVersion).getOrElse("SNAPSHOT")

  def addConfig(config: Config, settings: (String, Any)*): Config =
    ConfigFactory.parseMap(Map(settings: _*).asJava).withFallback(config)

  val builder = OParser.builder[Config]

  val argParser = {
    import builder._
    OParser.sequence(
      programName("elasticTool"),
      version('v', "version"),
      help('h', "help"),
      head("TheHive migration tool", getVersion),
      opt[File]('c', "config")
        .valueName("<file>")
        .action((f, c) ⇒ ConfigFactory.parseFileAnySyntax(f).withFallback(c))
        .text("TheHive3 configuration file"),
      opt[String]('u', "es-uri")
        .valueName("http://ip1:port,ip2:port")
        .text("TheHive3 ElasticSearch URI")
        .action((u, c) ⇒ addConfig(c, "search.uri" → u)),
      opt[String]('i', "es-index")
        .valueName("<index>")
        .text("TheHive3 ElasticSearch index name")
        .action((i, c) ⇒ addConfig(c, "intput.search.index" → i)),
      opt[Unit]('l', "list-indexes")
        .action((_, c) ⇒ addConfig(c, "action.list" → true))
    )
  }

  val defaultConfig =
    ConfigFactory.defaultApplication()
//      .parseResources("play/reference-overrides.conf")
//      .withFallback(ConfigFactory.defaultReference())
//      .resolve()
  OParser.parse(argParser, args, defaultConfig).foreach { config ⇒
    val client = new ElasticSearchClient(Configuration(config))
    if (config.getBoolean("action.list")) {
      println("Indexes:")
      client.listExtendedIndexes.map(println)
    }
  }

  System.exit(0)
}
