package net.surguy.olympic

import org.apache.commons.io.IOUtils
import java.io.FileInputStream
import scala.collection.jcl.Conversions._


/**
 * Use a list of common names loaded from a file to determine whether a name is male or female.
 * Uses names taken from http://www.ssa.gov/OACT/babynames/decades/names1960s.html
 */
object GenderFinder {
    val boysNames = loadNamesFromFile("boys.txt")
    val girlsNames = loadNamesFromFile("girls.txt")

    def loadNamesFromFile(filename:String) = {
        val list = IOUtils.readLines(new FileInputStream(filename)).asInstanceOf[java.util.List[String]]
        list.map(s => s.toLowerCase.trim )
    }

    def isMale(firstName:String) = {
        val name = firstName.toLowerCase().trim()
        val gender = (boysNames.contains(name), girlsNames.contains(name))
        gender match {
          // If in both lists, decide gender based on which list it appears in first (i.e. most popular)
          case (true, true) => boysNames.indexOf(name) < girlsNames.indexOf(name)
          case (true, _) => true
          case (_, true) => false
          // If in neither list, assume male unless it ends in a a,e,i, or y (this is usually correct)
          case (_, _) => ! List('a','e','i','y').contains(name(name.length-1))

        }
    }
}

import org.specs._

object GenderFinderSpec extends Specification {
  "gender" should {
    "support common male names" in { GenderFinder.isMale("Charles") mustBe true }
    "support common female names" in { GenderFinder.isMale("Sarah") mustBe false }
    "support indeterminate names" in { GenderFinder.isMale("Kim") mustBe true }
    "support unusual female names" in { GenderFinder.isMale("Bryony") mustBe false }
    "support unusual male names" in { GenderFinder.isMale("Kal") mustBe true }
  }
}
