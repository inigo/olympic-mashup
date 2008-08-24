package net.surguy.olympic

import com.gargoylesoftware.htmlunit.{Page, WebClient}
import com.gargoylesoftware.htmlunit.xml.XmlPage
import com.hp.hpl.jena.ontology._
import com.hp.hpl.jena.rdf.model._
import java.net.URLEncoder

object DbpediaFinder {
  val endPoint = "http://dbpedia.org/sparql?default-graph-uri=http%3A%2F%2Fdbpedia.org&query="

  val prefix = "PREFIX owl: <http://www.w3.org/2002/07/owl#>\n"+
            "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n"+
            "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"+
            "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"+
            "PREFIX foaf: <http://xmlns.com/foaf/0.1/>\n"+
            "PREFIX dc: <http://purl.org/dc/elements/1.1/>\n"+
            "PREFIX : <http://dbpedia.org/resource/>\n"+
            "PREFIX dbpedia2: <http://dbpedia.org/property/>\n"+
            "PREFIX dbpedia: <http://dbpedia.org/>\n"+
            "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>\n"


  def lookupCountry(name : String, fields : List[String]) = {
    // We look up each element individually, rather than combining them into one query,
    // to simplify the query creation logic
    val model = ModelFactory.createDefaultModel()
    fields.foreach(f => {
          try {
              val construct = getConstruct(toUri(adjustCountryName(name)), f)
              val query = endPoint + URLEncoder.encode(prefix+construct)
              model.read(query)
          } catch {
            case _ => println("Failed to download")
          }
    })

    if (model.size==0) {
      println("Failed to retrieve statements for "+name+" : URI used is "+toUri(adjustCountryName(name)))
    }

    model
  }

  def toUri(name:String) = "<http://dbpedia.org/resource/"+name.replaceAll("[^0-9A-z\\'\\/\\(\\)\\%]","_")+">"

  def getConstruct(uri : String, field : String) = {
    val items = uri+ " "+field+" "+" ?val \r\n"
    String.format("CONSTRUCT { %s } WHERE { %s }", Array( items, items ) )
  }

  def adjustCountryName(country : String) = {
    country match {
      case "China" => "People%27s_Republic_of_China"  
      case "Russian Fed." => "Russia"
      case "Great Britain" => "United_Kingdom"
      case "DPR Korea" => "Korea"
      case "Trinidad/Tobago" => "Trinidad and Tobago"
      case "Georgia" => "Georgia_(country)" // Doesn't work
      case "Chinese Taipei" => "Taipei" // Doesn't work
      case _ => country
    }
  }


  def main (args: Array[String]) {
      lookupCountry("Germany", List("dbpedia2:areaKm")).write(System.out)
      lookupCountry("United States", List("dbpedia2:areaKm")).write(System.out)
  }

}