package net.surguy.olympic

import com.hp.hpl.jena.rdf.model.impl.LiteralImpl
import java.io.{FileInputStream, FileOutputStream, FileWriter}
import com.hp.hpl.jena.util.iterator.ExtendedIterator
import com.hp.hpl.jena.ontology._
import com.hp.hpl.jena.rdf.model._

import scala.collection.jcl.Conversions
import scala.Predef

/**  Convert the contents of all the combined RDF models to JSON that is compatible with exhibit */
object JsonExporter {
  val baseURI = "http://surguy.net/olympics/"

  def exportToJson(sourceRdf : List[String], outputJson : String) = {
    val sourceModel : OntModel = ModelFactory.createOntologyModel()
    sourceRdf.foreach( f => sourceModel.read(new FileInputStream(f),"") )

    val athleteResource = ResourceFactory.createResource(baseURI + "athlete")
    val individuals = sourceModel.listIndividuals(athleteResource)

    var json = new StringBuilder()
    json.append("{ items: [ \n")

    while(individuals.hasNext) {
      val individual : Individual = individuals.next().asInstanceOf[Individual];
      json.append(" { \n")

      var separator = "\n"
      val properties = individual.listProperties()
      while (properties.hasNext) {
        val property : Statement = properties.next().asInstanceOf[Statement]
        val label = property.getPredicate.getLocalName
        val o = property.getObject
        val value = if (o.isLiteral) property.getObject.asInstanceOf[Literal].getString else o.toString
        json.append(separator+label+" : "+"\""+value+"\"")
        separator=",\n"
      }
      json.append(" }, \n")
    }
    json.append("]")

    json.append(", properties: { 'weight': { valueType: \"number\" }, 'height': { valueType: \"number\" } }")

    json.append("}\n")


    writeToFile(json.toString, outputJson)
    println(json)
  }

  def writeToFile(text: String, filename : String)= {
    var writer = new FileWriter(filename)
    writer.write(text)
    writer.close()
  }

  def main (args: Array[String]) {
     exportToJson(List("athletes.rdf", "athlete_birthDates.rdf", "athlete_geocoded.rdf",
       "athlete_labelled.rdf", "athlete_genders.rdf"),
       "/Users/inigosurguy/Sites/athletes.json")
   }
}


