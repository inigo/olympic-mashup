package net.surguy.olympic

import com.hp.hpl.jena.ontology._
import com.hp.hpl.jena.rdf.model._
import java.io.{BufferedReader, FileOutputStream, FileWriter, InputStream}
import com.gargoylesoftware.htmlunit.html._
import com.gargoylesoftware.htmlunit._
import java.net.URL

import scala.collection.jcl.Conversions._
import pimpmyjena.Conversions._

/** Download medal data from the Beijing Olympics site */
object MedalScraper extends IdCreator(2000) with ModelUser {

  val startPage = "http://results.beijing2008.cn/WRM/ENG/INF/AR/C93/AR0000000.shtml"
  val basePage = "http://results.beijing2008.cn/"

  val model = ModelFactory.createOntologyModel()
  val medalClass = model.createClass(baseURI + "medal")
  
  def downloadPage(url: String) = {
    val attrs = getPage(url).getByXPath("//select[@id='selId']/option/@value")
    for (i <- (0 until attrs.size)) {
      val attr = attrs.get(i).asInstanceOf[DomAttr]
      processMedals(basePage + attr.getValue)
    }
  }

  def processMedals(url : String) = {
    val page = getPage(url)
    val sport = page.getByXPath("//div[@class='title']/div[@class='t2']/text()").get(0).toString

    val rows = page.getByXPath("(//table)[2]//tr[@class!='tdTitle']")

    var currentDate = ""
    for (i <- (0 until rows.size)) {
      val row : HtmlTableRow = rows.get(i).asInstanceOf[HtmlTableRow]
      val date = row.getCell(1).asText.trim
      if (date!="") currentDate = date

      val m = row.getCell(2).asXml
      if (m.contains("img")) {
        val medal = m.substring(m.indexOf("images/")+7, m.indexOf("M.gif"))
        val country = row.getCell(row.getCells.length - 1).asText

        val individual = model.createIndividual(baseURI +createID(), medalClass)
        individual("medal") = medal
        individual("country") = country
        individual("date") = currentDate
        individual("sport") = sport
        println(medal+" medal for "+sport+" won by "+country+" on "+currentDate)
      }
    }

  }

  def getPage(url: String) = {
    val webClient = new WebClient()
    webClient.setJavaScriptEnabled(false)
    webClient.getPage(url).asInstanceOf[HtmlPage]
  }

  def main (args: Array[String]) {
    downloadPage(startPage)
    writeModel(model, "medals.rdf")
  }
}

