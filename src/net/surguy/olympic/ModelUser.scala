package net.surguy.olympic

import java.io.{File, FileInputStream, FileOutputStream}
import com.hp.hpl.jena.ontology._
import com.hp.hpl.jena.rdf.model._
import scala.collection.jcl.Conversions._


/** Create a stream of IDs - intended for creating individuals. */
class IdCreator(var id : Int) {
  def createID() = { id = id + 1; ""+id }
}

/** Common methods used for reading and writing to RDF models. */
trait ModelUser {
  val baseURI = "http://surguy.net/olympics/"

  def readModel(sourceRdf : String) : OntModel = {
    val sourceModel = ModelFactory.createOntologyModel()
    sourceModel.read(new FileInputStream(sourceRdf),baseURI)
    sourceModel
  }

  def getFullModel() = {
    val model = ModelFactory.createOntologyModel()
    new File(".").listFiles().filter(_.getName.endsWith(".rdf")).
            foreach( f => model.read(new FileInputStream(f), baseURI) )
    model
  }

  def writeModel(model: Model, rdfFile:String) = {
    var out = new FileOutputStream(rdfFile)
    model.setNsPrefix("",baseURI)
    model.write(out, "RDF/XML-ABBREV")
    out.close()
  }
}
