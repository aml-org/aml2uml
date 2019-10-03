package aml.uml

import java.nio.file.Paths

import amf.core.annotations.{Aliases, DeclaredElement}
import amf.core.model.document.{BaseUnit, Module}
import amf.core.model.domain.Shape
import amf.core.model.domain.extensions.PropertyShape
import amf.plugins.document.vocabularies.model.document.{Dialect, DialectFragment, DialectLibrary}
import amf.plugins.document.vocabularies.model.domain.NodeMapping
import amf.plugins.domain.shapes.models.{ArrayShape, NodeShape, ScalarShape, UnionShape}

import scala.collection.mutable

class ShapesParser(dialectUnit: BaseUnit) {

  case class ForeignLink(source: NodeShape, alias: String, foreignMapping: NodeMapping)

  protected val nodeMap: mutable.Map[String, NodeShape] = mutable.Map()
  protected val foreignLinks: mutable.ListBuffer[ForeignLink] = mutable.ListBuffer()
  protected var c: Int = 0

  /**
   * Set correctly the alias of a foreign link when generating a RAML datatype
   */
  def generateRamlForeignLinks() = {
    foreignLinks.foreach { case ForeignLink(source, alias, foreignMapping) =>
        source.withLinkLabel(s"${alias}.${foreignMapping.name.value()}")
    }
  }

  /**
   * Set correctly the $ref of a foreign link when generating a JSON-Schema
   */
  def generateJsonSchemaLinks() = {
    foreignLinks.foreach { case ForeignLink(source, _, foreignMapping) =>
      val path = foreignMapping.id.split("#").last.replace("declarations", "definitions")
      val label = relativePaths(source.id.split("#").head, foreignMapping.id.split("#").head).replace(".yaml", ".json") + "#" + path
      source.withLinkLabel(label)
    }
  }

  /**
   * Main entry point to trigger the generation of RAML datatypes and JSON schemas in the CIM directory
   * @param location
   * @param id
   * @param usage
   * @param referenced
   * @return
   */
  def generate(location: String, id: Option[String], usage: Option[String], referenced: Boolean = false): Module = {
    val moduleId = id.getOrElse("http://aml2dt.com/Module")
    val module = Module().withLocation(location).withId(moduleId)
    usage.foreach(module.withUsage)

    val declarations = collectUnits
    declarations.foreach(parseNodeMappingWithoutProperties)
    declarations.foreach(parseNodeMappingProperties)

    module.withDeclares(nodeMap.values.toSeq)

    dialectUnit.annotations.find(classOf[Aliases]) map { aliases =>
        module.annotations += aliases
    }

    if (!referenced) {
      val references = dialectUnit.references.map(d => new ShapesParser(d).generate(d.location().get, Some(d.id), d.usage.option(), true))
      module.withReferences(references)
    }

    module
  }

  protected def collectUnits: Seq[NodeMapping] = {
    val domainElements = dialectUnit match {
      case d: Dialect =>
        Option(d.encodes).map(Seq(_)).getOrElse(Nil) ++ d.declares
      case d: DialectLibrary =>
        d.declares
      case d: DialectFragment =>
        Option(d.encodes).map(Seq(_)).getOrElse(Nil)
    }

    domainElements.collect { case x if x.isInstanceOf[NodeMapping] => x.asInstanceOf[NodeMapping] }
  }

  protected def parseNodeMappingWithoutProperties(nodeMapping: NodeMapping): NodeShape = {
    val shape = NodeShape().withId(nodeMapping.id)

    shape.annotations += DeclaredElement()

    nodeMapping.name.option() match {
      case Some(name) => shape.withName(name)
      case _          => // ignore
    }

    nodeMap.put(nodeMapping.id, shape)
    shape
  }

  protected def parseNodeMappingProperties(nodeMapping: NodeMapping): Shape = {
    val shape = nodeMap(nodeMapping.id)

    val propertyShapes = nodeMapping.propertiesMapping() map { propertyMapping =>
      val propertyShape = PropertyShape().withId(propertyMapping.id)
      propertyMapping.name().option() match {
        case Some(name) => propertyShape.withName(name)
        case _          => // ignore
      }
      propertyMapping.minCount().option().foreach(propertyShape.withMinCount)
      propertyMapping.literalRange().option().foreach { dataType =>
        val scalar = ScalarShape().withDataType(dataType)
        if (propertyMapping.allowMultiple().option().getOrElse(false)) {
          c += 1
          val array = ArrayShape().withId(s"http://aml2dt.com/autogen${c}").withName(s"Array${shape.name.value()}").withItems(scalar)
          propertyShape.withRange(array)
        } else {
          propertyShape.withRange(scalar)
        }
      }

      val objectRangeIds = propertyMapping.objectRange().collect { case range if range.option().isDefined => range.value() }
      val objectRanges: Seq[Shape] = objectRangeIds.map { id =>
        val fallbackShape: Shape  = NodeShape().withId(id).link(id)
        nodeMap.get(id) match {
          case Some(targetShape) => targetShape.link(targetShape.name.value()).asInstanceOf[Shape]
          case _                 => {
            // foreign link
            val maybeForeignDialect = dialectUnit.references.find { case ref: Dialect =>
              ref.declares.exists(_.id == id)
            }
            val maybeForeignShape = maybeForeignDialect.flatMap { case dialect:Dialect =>
              dialect.declares.find(_.id == id)
            }

            (maybeForeignDialect, maybeForeignShape) match {
              case (Some(foreignDialect: Dialect), Some(foreignShape: NodeMapping)) =>
                dialectUnit.annotations.find(classOf[Aliases]) map { aliases: Aliases =>
                  val maybeAlias = aliases.aliases.find { case (alias, (fullUrl, _)) =>
                      fullUrl == foreignDialect.id
                  }
                  val s:Shape = maybeAlias match {
                    case Some((alias, _)) =>
                      val foreignNode: NodeShape = NodeShape().withId(id).link(foreignShape.id) // temporary id, will be replaced later using the stored link
                      foreignLinks += ForeignLink(foreignNode, alias, foreignShape)
                      foreignNode
                    case _              =>
                      fallbackShape
                  }
                  s
                } getOrElse(fallbackShape)

              case _ =>
                fallbackShape
            }
          }
        }

      }

      val objRange = if (objectRanges.length == 1) {
        Some(objectRanges.head)
      } else if (objectRanges.length > 1) {
        c += 1
        Some(UnionShape().withId(s"http://aml2dt.com/autogen/${c}").withName(s"Union${c}").withAnyOf(objectRanges))
      } else None

      objRange.foreach { shape =>
        if (propertyMapping.allowMultiple().option().getOrElse(false)) {
          c += 1
          val array = ArrayShape().withId(s"http://aml2dt.com/autogen${c}").withName(s"Array${shape.name.value()}").withItems(shape)
          propertyShape.withRange(array)
        } else {
          propertyShape.withRange(shape)
        }
      }

      propertyShape
    }

    shape.withProperties(propertyShapes)
  }

  protected def relativePaths(from: String, to: String): String = {
    Paths.get(from).getParent.relativize(Paths.get(to)).toString
  }


}
