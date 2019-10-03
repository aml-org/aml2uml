package amf.core.emitter

import amf.core.model.document.{BaseUnit, Module}
import amf.core.parser.{ErrorHandler, Position}
import amf.plugins.document.webapi.JsonSchemaPlugin
import amf.plugins.document.webapi.contexts.JsonSchemaEmitterContext
import amf.plugins.document.webapi.parser.spec.OasDefinitions
import amf.plugins.document.webapi.parser.spec.common.JsonSchemaEntry
import amf.plugins.document.webapi.parser.spec.oas.OasDeclarationsEmitter
import amf.plugins.domain.shapes.models.AnyShape
import org.yaml.model.YDocument
import org.yaml.model.YDocument.EntryBuilder


case class JsonSchemaEmitterLibrary(shapes: Seq[AnyShape], ordering: SpecOrdering = SpecOrdering.Lexical, eh: ErrorHandler) {
  def emitDocument(): YDocument = {
    YDocument(b => {
      b.obj { b =>
        amf.core.emitter.BaseEmitters.traverse(emitters, b)
      }
    })
  }

  private val jsonSchemaRefEntry = new EntryEmitter {
    override def emit(b: EntryBuilder): Unit =
      b.entry("oneOf", (e) => {
        e.list((l) => {
          shapes.foreach { s =>
            l.obj((o) => {
              o.entry("$ref", OasDefinitions.appendDefinitionsPrefix(s.name.value()))
            })
          }
        })
      })

    override def position(): Position = Position.ZERO
  }

  private def sortedTypeEntries =
    ordering.sorted(
      OasDeclarationsEmitter(shapes, SpecOrdering.Lexical, Seq())(JsonSchemaEmitterContext(eh)).emitters) // spec 3 context? or 2? set from outside, from vendor?? support two versions of jsonSchema??

  private val emitters = Seq(JsonSchemaEntry, jsonSchemaRefEntry) ++ sortedTypeEntries
}


class JsonSchemaLibraryPlugin extends JsonSchemaPlugin {

  override val ID: String = "JSON-Schema Library" // version?

  /**
   * List of media types used to encode serialisations of
   * this domain
   */
  override def documentSyntaxes: Seq[String] = Seq("application/library+schema+json", "application/library+payload+json")

  override protected def unparseAsYDocument(unit: BaseUnit, renderOptions: RenderOptions): Option[YDocument] = {
    unit match {
      case module: Module =>
        val shapes = module.declares.collect{ case a: AnyShape => a }
        Some(JsonSchemaEmitterLibrary(shapes, eh = renderOptions.errorHandler).emitDocument())
      case _              =>
        None
    }

  }

}

object JsonSchemaLibraryPlugin extends JsonSchemaLibraryPlugin