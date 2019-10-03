package aml.uml

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import amf.core.AMF
import amf.core.metamodel.Field
import amf.core.model.document.BaseUnit
import amf.core.parser.UnspecifiedReference
import amf.core.remote.{Cache, Context}
import amf.core.services.RuntimeCompiler

import scala.collection.JavaConverters._
import amf.core.unsafe.PlatformSecrets
import amf.plugins.document.vocabularies.AMLPlugin
import amf.plugins.document.vocabularies.model.document.Dialect
import amf.plugins.document.vocabularies.model.domain.{NodeMapping, PropertyMapping}
import amf.plugins.document.webapi.{Oas20Plugin, Raml10Plugin}
import aml.cim.Generator

import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object RepositoryLoader extends PlatformSecrets {
  val managedProperties = List("id","systemModstamp", "externalRecordId", "createdDate", "lastModifiedDate")

  def loadDialect(path: String): Future[BaseUnit] = {
    for {
      _      <- {
        AMF.registerPlugin(AMLPlugin)
        AMF.registerPlugin(Raml10Plugin)
        AMF.registerPlugin(Oas20Plugin)
        AMF.init()
      }
      parsed <- RuntimeCompiler(path,
        Some("application/yaml"),
        Some("AML 1.0"),
        Context(platform),
        UnspecifiedReference,
        Cache())
    } yield {
      parsed
    }
  }

  def fromDirectory(path: String) = {
    var files = Files.walk(Paths.get(path)).iterator().asScala
    val schemaFiles = files.filter(f => f.endsWith("schema.yaml"))
    files = Files.walk(Paths.get(path)).iterator().asScala
    val futures = schemaFiles.map { f =>
      processSchemaFile(f).map { a =>
        (f,a)
        //println(s"*** Processed ${f}")
      } recover {
        case e: Exception =>
          println(s"failed!!! ${e.getMessage}")
          e.printStackTrace()
          null //List[(Path,Classy)]()
      }
    }
    Future.sequence(futures)
  }

  def afterSlash(uri :String) = uri.substring(uri.lastIndexOf("/") + 1)
  def preSlash(uri : String): String = uri.substring(0,uri.lastIndexOf("/"))
  def afterHash(uri :String) = uri.substring(uri.lastIndexOf("#") + 1)

  case class Attribute(name : String, typeo : String)
  case class Relation(name: String, other : String)
  case class PackageDependency(pack : String, name: String, other: String)
  case class Classy(name : String, attributes : List[Attribute], relations : List[Relation],
                       packages : List[PackageDependency], parent : String)
  val classy = new mutable.ListBuffer[Classy]()

  protected def processSchemaFile(f: Path):Future[List[Classy]] = {
    println(s"*** Loading schema file ${f.toFile.getAbsolutePath}")
    val dialectList = loadDialect("file://" + f.toFile.getAbsolutePath)
    dialectList.map { case dialect: Dialect =>
      val objectOrProperty = new mutable.HashMap[String,Boolean]()
      val localOrRemote  = new mutable.HashMap[String,Boolean]()
      var reduced = List[PropertyMapping]()
      var inNamespaceMap = dialect.declares.map(z => (afterSlash(z.id),true)).toMap
      dialect.declares.foreach(z => {
        reduced = z.asInstanceOf[NodeMapping].propertiesMapping().toList.filter(a => {
          !managedProperties.contains(afterSlash(a.id))
        })
        if (reduced.length > 2){
          objectOrProperty.put(z.id,true)
        } else {
          objectOrProperty.put(z.id, false)
        }
      })
      dialect.declares.filter(z => objectOrProperty(z.id)).map(z => {
        reduced = z.asInstanceOf[NodeMapping].propertiesMapping().toList.filter(a => {
          !managedProperties.contains(afterSlash(a.id))
        })
        if (reduced.length > 2){
          objectOrProperty.put(z.id,true)
        } else {
          objectOrProperty.put(z.id, false)
        }
        var oughtaAttribs = List[Attribute]()
        var oughtaAssocs = List[Relation]()
        var oughtaDepends = List[PackageDependency]()
        val extList = z.extend.toList
        val parent = if (extList.length > 0) {
          extList.head.asInstanceOf[NodeMapping].linkTarget match {
            case Some(value) => afterSlash(value.id)
            case None => null
          }
        } else null
        try {
          /* "real" attributes
          reduced = z.asInstanceOf[NodeMapping].propertiesMapping().toList.filter(a => {
            !managedProperties.contains(afterSlash(a.id))
          })
           */
          reduced.foreach(p => {
            p.objectRange().toList match {
              case fld :: _ => fld.option() match {
                case Some(value) => dialect.findById(value) match {
                  case Some(_) => localOrRemote.put(value,true)
                  case None => localOrRemote.put(value, false)
                }
                case None => ()
              }
              case List() => ()
            }
          })
          if (objectOrProperty(z.id)){
            oughtaAttribs = reduced.foldLeft(List[Attribute]())((atts,n) => {
              n.literalRange().option() match {
                case Some(value) =>
                  var spot = Math.max(value.lastIndexOf("#") + 1,0)
                  Attribute(afterSlash(n.id),value.substring(spot)) :: atts
                case None => n.objectRange().headOption match {
                  case Some(value) => if (!objectOrProperty.getOrElse(n.id,true)) {
                    Attribute(afterSlash(n.id),afterSlash(value.value())) :: atts
                  } else atts
                  case None => throw new Exception("item "+n.id+" has neither literal nor object")
                }
              }
            })
            val foo = reduced.foldLeft((List[Relation](),List[PackageDependency]()))((sb,n) => {
              n.objectRange().headOption match {
                case Some(value) =>
                  if (objectOrProperty.getOrElse(value.value(),true)) {
                    inNamespaceMap.getOrElse(afterSlash(value.value()), false) match {
                      case true =>
                        (Relation(afterSlash(n.id),afterSlash(value.value())) :: sb._1, sb._2)
                      case false =>
                        dialect.references.
                          find(_.asInstanceOf[Dialect].declares.exists(nm => nm.id == value.value())) match {
                          case Some(otherDialect) =>
                            (sb._1, PackageDependency(afterSlash(preSlash(otherDialect.id)), afterSlash(n.id),
                              afterSlash(afterSlash(value.value()))) :: sb._2)
                          case None => sb
                        }
                    }
                  } else sb
                case None => sb
              }
            })
            oughtaAssocs = foo._1
            oughtaDepends = foo._2
          }
        } catch {
          case e:Exception => {
            e.printStackTrace();
            null
          }
        }
        Classy(afterSlash(z.id),oughtaAttribs,oughtaAssocs,oughtaDepends,parent)
      })(collection.breakOut)
    }
    //val dialect = Await.result(, Duration.Inf).asInstanceOf[Dialect]
  }

  protected def writeFile(filename: String, text: String) = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text)
    bw.close()
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Path to a directory containing CIM files must be provided as an argument")
      System.exit(1)
    }
    val path = args(0)
    println(s"\n\nProcessing directory $path\n\n")
    val result = fromDirectory(path)
    val dones = Await.result(result, Duration.Inf)
    /*
    dones.foreach(aschema => {
      val filePath = aschema._1
      val prepath = filePath.getParent
      val ending = filePath.getFileName.toString
      val newPath = prepath.resolve("umlDiagram.scruffy")
      val classy = aschema._2
      val p1 = classy.flatMap(_.packages.map(_.pack)).sorted.foldLeft(List[String]())((pl, p) =>
        if (pl.isEmpty || p != pl.head) {
          p :: pl
        } else {
          pl
        }).foldLeft(new mutable.StringBuilder())((s, p) => s ++= "[eg." + p + "],")



      val p2 = classy.foldLeft(new mutable.StringBuilder())((sb, c) => {
        sb ++= ("[" + c.name + "|")
        val sb2 = (if (c.attributes.isEmpty) sb
                   else (c.attributes.foldLeft(sb)((b, a) => b ++= a.name ++= ":" ++= a.typeo ++= ";"))).dropRight(1) ++= "],"
        c.relations.foreach(r => sb2 ++= ("["+c.name + "]++-"+r.name+">[" + r.other + "],"))
        if (c.parent != null) {
          sb2 ++= ("["+c.parent + "]^[" + c.name + "],")
        }
        c.packages.map(p => p.pack).sorted.foldLeft("")((p, o) => {
          if (p != o) {
            sb2 ++= ("["+c.name + "]->[eg." + o + "],")
          }
          o
        })
        sb2
      })
      val w = Files.newBufferedWriter(newPath)
      w.write(p2.dropRight(1).result())
      w.close()
      --
       */
    dones.foreach(aschema => {
      val filePath = aschema._1
      val prepath = filePath.getParent
      val ending = filePath.getFileName.toString
      val newPath = prepath.resolve("diagram.uml")
      val classy = aschema._2
      val p1 = classy.flatMap(_.packages.map(_.pack)).sorted.foldLeft(List[String]())((pl, p) =>
        if (pl.isEmpty || p != pl.head) {
          p :: pl
        } else {
          pl
        }).foldLeft(new mutable.StringBuilder() ++= "@startuml\n")((s, p) => s ++= "package eg." + p + " {}\n")
      val p2 = classy.foldLeft(p1)((sb,c) => {
        sb ++= ("class " + c.name + " {\n")
        (if (c.attributes.length == 0) sb
        else (c.attributes.foldLeft(sb)((b, a) => b ++= a.typeo ++= " " ++= a.name ++= "\n"))) ++= "}\n"
        c.relations.foreach(r => sb ++= c.name + " *-- " + r.other + " : " + r.name + "\n")
        if (c.parent != null) {
          sb ++= (c.name + " --|> " + c.parent + "\n")
        }
        c.packages.map(p => p.pack).sorted.foldLeft("")((p, o) => {
          if (p != o) {
            sb ++= (c.name + " *-- eg." + o + "\n")
          }
          o
        })
        sb
      })
      p2 ++= "@enduml"
      val w = Files.newBufferedWriter(newPath)
      w.write(p2.result())
      w.close()

      /*
    classy.foreach(c => {
      var sb = new mutable.StringBuilder()
      sb ++= ("[ "+c.name+" | ")
      sb = (if (c.attributes.length == 0) sb
               else (c.attributes.foldLeft(sb)((b,a) => b ++= a.name ++= " : " ++= a.typeo ++= " ; "))).dropRight(2) ++= "]\n"
      c.relations.foreach(r => sb ++= "[ "+c.name+"] "+r.name+" o-> ["+r.other+" ]\n")
      if (c.parent != null){
        sb ++= ("[ "+c.name+" ] -:> [ "+c.parent+" ]\n")
      }
      c.packages.map(p => p.pack).sorted.foldLeft("")((p,o) => {
        if (p != o) {
          sb ++= ("[ "+c.name+" ] -> [<package> "+o+" ]\n")
        }
        o
      })
      println(sb.result())
    })

       */


    })
  }
}
