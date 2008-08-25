package net.surguy.olympic

import java.io.FileOutputStream
import com.hp.hpl.jena.ontology._
import com.hp.hpl.jena.rdf.model._
import scala.xml._

/** Convert simple XML to be stored in an Jena RDF model. */
object XmlToJenaConverter extends IdCreator(1000) with ModelUser {
  def xmlToRdf(xmlFile : String, rdfFile : String) = {
    val athletes = XML.load(xmlFile)
    val model = ModelFactory.createOntologyModel()
    val athleteClass = model.createClass(baseURI + "athlete")

    for (val athlete : Node <- athletes \\ "athlete") {
        val individual : Individual = model.createIndividual(baseURI +createID(), athleteClass)
        athlete.child.elements.filter( e => e.isInstanceOf[Elem]).
                foreach(e => {
                        val elem = e.asInstanceOf[Elem]
                        val prop = ResourceFactory.createProperty(baseURI+elem.label)
                        individual.addProperty(prop, e.text.trim())
                })
    }

    writeModel(model, rdfFile)
  }

  def main (args: Array[String]) {
    xmlToRdf("athletes.xml", "athletes.rdf")
  }

}