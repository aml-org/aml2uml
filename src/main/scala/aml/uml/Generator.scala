package aml.cim

import amf.client.model.document.Module
import amf.client.render.{Raml10Renderer, Renderer}
import amf.core.emitter.JsonSchemaLibraryPlugin
import amf.core.registries.AMFPluginsRegistry
import amf.plugins.document.vocabularies.model.document.Dialect
import aml.uml.ShapesParser

class Generator(dialect: Dialect) extends Renderer("JSON-Schema Library", "application/library+schema+json") {
  AMFPluginsRegistry.registerDocumentPlugin(JsonSchemaLibraryPlugin)

  def generate(format: String): String = {
    val parser = new ShapesParser(dialect)
    val module = parser.generate(dialect.location().get, Some(dialect.id), dialect.usage.option())
    format match {
      case "RAML 1.0" =>
        parser.generateRamlForeignLinks()
        new Raml10Renderer().generateString(Module(module)).get
      case "JSON-Schema" =>
        parser.generateJsonSchemaLinks()
        generateString(Module(module)).get
      case _             =>
        throw new Exception(s"Unknown format ${format}")
    }
  }

}
