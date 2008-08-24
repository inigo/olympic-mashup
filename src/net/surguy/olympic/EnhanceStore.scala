package net.surguy.olympic

import java.io.{File, FileInputStream, FileOutputStream, InputStream}
import java.util.ArrayList
import java.text.SimpleDateFormat
import com.hp.hpl.jena.ontology._
import com.hp.hpl.jena.rdf.model._

import scala.collection.jcl.Conversions._
import pimpmyjena.Conversions._

/** Adds a country object property to every medal with a country field */
object ObjectPropertyTransformer extends IdCreator(3000) with ModelUser {
  val countryClass = ResourceFactory.createProperty(baseURI, "Country")

  def transform() = {
    val sourceModel = readModel("medals.rdf")
    val annotationModel = ModelFactory.createOntologyModel()
    val newClassesModel = ModelFactory.createOntologyModel()

    sourceModel.individuals("medal").foreach( ind => {
      val newInd = annotationModel.createIndividual( ind.getURI, ind.getOntClass )
      val country = ind("country").toString
      val existingCountry : Option[Individual] = newClassesModel.individuals("Country").
              filter( ind => ind("name").toString==country ).firstOption
      if (existingCountry.isDefined) {
        newInd("countryObject") = existingCountry.get
      } else {
        val newCountry = newClassesModel.createIndividual( baseURI+createID(), countryClass )
        newCountry("name") = country
        newInd("countryObject") = newCountry
      }
    })

    writeModel(annotationModel, "medals_countries.rdf")
    writeModel(newClassesModel, "countries.rdf")
  }
}

/** Retrieve information from DBPedia. */
object DbpediaEnhancer extends Enhancer("Country") {
  def enhanceIndividual(ind : Individual, newInd : Individual) = {
      val name = ind("name").toString
      val fields=List( "dbpedia2:populationEstimate", "dbpedia2:gdpNominalPerCapita", "dbpedia2:areaKm",
            "dbpedia2:populationDensityKm", "dbpedia2:populationCensus", "dbpedia2:gdpPppPerCapita",
            "dbpedia2:gdpNominal", "dbpedia2:gdpNominalPerCapita", "dbpedia2:hdi")
      var m : Model = DbpediaFinder.lookupCountry(name, fields)
      m.listStatements.toList.asInstanceOf[ArrayList[Statement]].foreach( (stmt:Statement) => {
        newInd( stmt.getPredicate.getLocalName ) = stmt.getObject.stringValue.replaceAll("[^\\d+\\.]","")
      })
  }
}

/** Add genders for the athletes - almost always correctly. */
object GenderEnhancer extends Enhancer("athlete") {
  def enhanceIndividual(ind : Individual, newInd : Individual) = {
      val name = ind("name").toString
      val firstName = name.split(" ")(0)
      newInd("gender") = if (GenderFinder.isMale(firstName)) "male" else "female"
  }
}

/** Add dates for medal winning. */
object MedalEnhancer extends Enhancer("medal") {
  def enhanceIndividual(ind : Individual, newInd : Individual) = {
      val date = ind("date").toString
      val day = Integer.parseInt(date.split(" ")(2)) - 8 // Games started on Sun Aug 9
      newInd("day") = ""+day
  }
}

/** Add ISO 8601 dates to the model based on the existing dd/MM/yy birth dates. */
object DateEnhancer extends Enhancer("athlete") {
  val ISO_8601 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZ")
  val DD_MM_YY = new SimpleDateFormat("dd/MM/yyyy")

  def enhanceIndividual(ind : Individual, newInd : Individual) = {
      val birthDate = ind("birthDate").toString
      newInd("isoDate") = ISO_8601.format(DD_MM_YY.parse(birthDate))
  }
}

/** Add rdfs:label annotations to the model using the name fields. */
object LabelEnhancer extends Enhancer("athlete") {
  def enhanceIndividual(ind : Individual, newInd : Individual) = {
      newInd.addLabel( ind("name") )
  }
}

/** Convert the placenames in the model to lat/long co-ordinates using the Google geocoder. */
object GeoEnhancer extends Enhancer("athlete") {
  val geoCoder = new GoogleGeocoder("Google API key removed for open source release - insert your own here")

  def enhanceIndividual(ind : Individual, newInd : Individual) = {
      val location = geoCoder.lookup( ind("lives").toString)
      newInd("liveLocation") = location.lat+","+location.long
  }
}

/** An abstract model enhancer, that runs through every individual athlete and applies a specific enhancement. */
abstract class Enhancer(val clazz:String) extends ModelUser {
  def enhanceIndividual(individual : Individual, newIndividual : Individual);

  def enhance(sourceRdf : String, outputRdf : String) = {
    val sourceModel = readModel(sourceRdf)
    val annotationModel = ModelFactory.createOntologyModel()
    sourceModel.individuals(clazz).foreach( ind => {
      val newInd = annotationModel.createIndividual( ind.getURI, ind.getOntClass )
      enhanceIndividual(ind, newInd)
    })
    writeModel(annotationModel, outputRdf)
  }
}


object RunEnhancers extends Application {
//     ObjectPropertyTransformer.transform()
     DbpediaEnhancer.enhance("countries.rdf", "countries_dbpedia.rdf")
//     GenderEnhancer.enhance("athletes.rdf", "athlete_genders.rdf")
//     DateEnhancer.enhance("athletes.rdf", "athlete_birthDates.rdf")
//     GeoEnhancer.enhance("athletes.rdf", "athlete_geocoded.rdf")
//     LabelEnhancer.enhance("athletes.rdf", "athlete_labelled.rdf")
//     MedalEnhancer.enhance("medals.rdf", "medal_days.rdf")
}
