package net.surguy.olympic

import scala.collection.mutable.{HashMap, ArrayBuffer, Map}
import scala.collection.immutable.HashSet
import scala.collection.Set
import com.hp.hpl.jena.rdf.model.impl.LiteralImpl
import java.io.{FileInputStream, FileOutputStream, FileWriter}
import com.hp.hpl.jena.util.iterator.ExtendedIterator
import com.hp.hpl.jena.ontology._
import com.hp.hpl.jena.rdf.model._

import scala.collection.jcl.Conversions
import scala.Predef

import pimpmyjena.Conversions._

/** Convert the medals table to a TSV file compatible with the Prefuse Flex displayer. */
object PrefuseExporter extends ModelUser {

  class DefaultDict[K, V](defaultFn: (K)=>V) extends HashMap[K, V] {
    override def default(key: K): V = return defaultFn(key)
  }

  def exportToPrefuse(outputFilename : String) = {
    val individuals :List[Individual] = getFullModel().individuals("medal").toList
    val sortedIndividuals  = individuals.sort( (i1, i2) => Integer.parseInt(i1("day").toString) < Integer.parseInt(i2("day").toString) )

    val gold = new DefaultDict[String, Int](K => 0)
    val silver = new DefaultDict[String, Int](K => 0)
    val bronze = new DefaultDict[String, Int](K => 0)

    val countries: Set[String] = Set() ++ individuals.map( ind => ind("country").toString )

    val out = new StringBuilder()
    out.append("day\tmedal\tcountry\tcount\n")

    var day = 1
    sortedIndividuals.foreach(ind => {
        val currentDay = Integer.parseInt( ind("day").toString )

        val country = ind("country").toString
        ind("medal").toString match {
          case "gold" => gold(country) = gold(country) + 1
          case "silver" => silver(country) = silver(country) + 1
          case "bronze" => bronze(country) = bronze(country) + 1
        }

        if (currentDay > day) {
          countries.foreach( c => {
            out.append(String.format("%s\t%s\t%s\t%s\n", Array(""+day, "1", c, ""+gold(c) )))
            out.append(String.format("%s\t%s\t%s\t%s\n", Array(""+day, "2", c, ""+silver(c) )))
            out.append(String.format("%s\t%s\t%s\t%s\n", Array(""+day, "3", c, ""+bronze(c) )))
          })
          day = currentDay
        }
    })

    writeToFile(out.toString, outputFilename)
    println(out)
  }

  def writeToFile(text: String, filename : String)= {
    var writer = new FileWriter(filename)
    writer.write(text)
    writer.close()
  }

  def main (args: Array[String]) {
    exportToPrefuse("medals.txt") 
  }
}



