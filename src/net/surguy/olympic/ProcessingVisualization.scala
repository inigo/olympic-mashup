package net.surguy.olympic

import scala.collection.mutable._
import java.io.FileInputStream
import java.awt.{Frame, BorderLayout}
import processing.core.PApplet
import processing.core.PConstants

import com.hp.hpl.jena.ontology._
import com.hp.hpl.jena.rdf.model._
import pimpmyjena.Conversions._

/**
 * Visualize the medal position of countries versus another numeric measure (e.g. population, area, GDP).
 * This is using the Processing library, and heavily based on an example from the book Visualizing Data by Ben Fry.
 */
class Processing extends PApplet with ModelUser {
  val bg = 0xEEEEEEEE
  val SIDE_PADDING = 30
  val ROW_SEPARATION = 18

//  val predicate = "areaKm"
//  val predicate = "hdi"
  val predicate = "gdpNominalPerCapita"
//  val title = "Medal position versus size of country (sq. km)"
//  val title = "Medal position versus human development index"
  val title = "Medal position versus nominal GDP per capita"

  val model = getFullModel()
  
  val countriesByMedal = getCountriesWithMedals().sort( (c1,c2) => c1.medalCount > c2.medalCount )
  val countriesByValue = getCountriesWithMedals().sort( (c1,c2) => c1.population > c2.population )

  // String formatting doesn't seem to be nice in the Scala standard library at the moment - this is from the mailing list
  def format(s: String, x: Any*) = String.format(s, x.asInstanceOf[scala.runtime.BoxedObjectArray].unbox(x.getClass).asInstanceOf[Array[Object]])

  override def setup() {
    size(600, (countriesByMedal.size * ROW_SEPARATION) + 50)
    fill(128)
    background(bg)
    smooth()

    textFont(createFont("Georgia", 14))
    textAlign(PConstants.CENTER, PConstants.TOP)
    text(title, 300, 5)

    textFont(createFont("Georgia", 12))


    translate(SIDE_PADDING, SIDE_PADDING)

    (0 until countriesByMedal.size).foreach( pos => {
      val current = countriesByMedal(pos)
      val other = countriesByValue(pos)
      val otherPos = countriesByValue.findIndexOf( c => c.name == current.name )

      val medalsY = pos*ROW_SEPARATION
      val otherY = otherPos*ROW_SEPARATION

      textAlign(PConstants.LEFT, PConstants.CENTER)
      text(current.name, 20, medalsY)
      textAlign(PConstants.RIGHT, PConstants.CENTER)
      text(current.medalCount, 130, medalsY)

      textAlign(PConstants.LEFT, PConstants.CENTER)
      val valDisplay = format("%,.0f", other.population)
      text(valDisplay, 350, medalsY)
      textAlign(PConstants.RIGHT, PConstants.CENTER)
      text(other.name, 500, medalsY)

      val weight = (Math.abs(pos - otherPos) / countriesByMedal.size.asInstanceOf[Float]) * 3 + 1
      strokeWeight(weight)
      if (pos<otherPos) stroke(33,85,156) else stroke(206,0,82)

      line(140, medalsY+4, 340, otherY+4)
    })

    save(predicate+".png")

  }

  class DefaultDict[K, V](defaultFn: (K)=>V) extends HashMap[K, V] {
    override def default(key: K): V = return defaultFn(key)
  }

  case class Country(name:String, medalCount:Int, population: Float)


  def getCountriesWithMedals() = {
    val medals = new DefaultDict[String, Int](K => 0)
    model.individuals("medal").foreach( ind => {
        val c = ind("country").toString
        medals( c ) = medals( c ) + 1
    })
    var countries = List[Country]()
    var i = 0
    medals.foreach( item => {
      val (countryName,medalCount) = item
      val value  = getValue(countryName, predicate)

      if (value!=0) {
        countries = Country(countryName, medalCount, value) :: countries
      }

    })
    countries
  }

  def getValue(countryName : String, predicate: String) : Float = {
    val value = model.individuals("Country").
            filter(ind => ind("name").toString == countryName && ind(predicate)!=null).firstOption
    value match {
      case Some(ind) => println(ind(predicate).toString); java.lang.Float.parseFloat(ind(predicate).toString)
      case None => 0.0f
    }
  }

}

object ProcessingTest extends Application {
  PApplet.main(Array[String]("net.surguy.olympic.Processing"))
}
  