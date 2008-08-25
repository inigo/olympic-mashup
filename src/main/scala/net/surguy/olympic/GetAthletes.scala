package net.surguy.olympic

import com.gargoylesoftware.htmlunit.html._
import java.io.{BufferedReader, FileWriter, InputStream}
import com.gargoylesoftware.htmlunit._

import java.net.URL

import scala.collection.jcl.Conversions
import scala.Predef

/** Download the data from the Team GB website to a simple XML format. */
object TeamGbScraper {

  val basePage = "http://www.olympics.org.uk/beijing2008/teamgb.aspx"
  val basePath = "http://www.olympics.org.uk/beijing2008/"

  def downloadPage(url: String) = {
    val webClient = new WebClient()
    webClient.setJavaScriptEnabled(false)
    val page : HtmlPage = webClient.getPage(url).asInstanceOf[HtmlPage]
    val links = page.getByXPath("//div[@id='searchAlpha']/a")
    for (i <- (0 until links.size)) {
      var a = links.get(i).asInstanceOf[HtmlAnchor];
      var s = a.getHrefAttribute()
      var value = s.substring(s.indexOf("'")+1, s.lastIndexOf("',"))

      var theForm = page.getFormByName("aspnetForm")
      var target = theForm.getInputByName("__EVENTTARGET")
      target.setValueAttribute(value)

      var newPage = theForm.submit(null).asInstanceOf[HtmlPage]
      processAlphabetPage(newPage)
    }
}

  def processAlphabetPage(page : HtmlPage) = {
    var athleteLinks = page.getByXPath("//div[@class='panelGB']/a")
    for (i <- (0 until athleteLinks.size)) {
      var a = athleteLinks.get(i).asInstanceOf[HtmlAnchor]
      var href = a.getHrefAttribute()
      processAthlete(href)
      Thread.sleep(500)
    }
  }

  def processAthlete(url : String) = {
    val webClient = new WebClient()
    webClient.setJavaScriptEnabled(false)
    val ath = webClient.getPage(basePath + url).asInstanceOf[HtmlPage]

    val name = retrieve(ath, "//div[@class='sport_spotlight']/h1/text()")
    val date = retrieve(ath, "//div[@class='sport_spotlight']//table//tr[1]//td[3]/text()")
    val lives = retrieve(ath, "//div[@class='sport_spotlight']//table//tr[2]//td[2]/text()")
    val born = retrieve(ath, "//div[@class='sport_spotlight']//table//tr[3]//td[2]/text()")
    val country = retrieve(ath, "//div[@class='sport_spotlight']//table//tr[4]//td[2]/text()")
    val height = retrieve(ath, "//div[@class='sport_spotlight']//table//tr[5]//td[2]/text()")
    val weight = retrieve(ath, "//div[@class='sport_spotlight']//table//tr[6]//td[2]/text()")
    val club = retrieve(ath, "//div[@class='sport_spotlight']//table//tr[7]//td[2]/text()")
    val coach = retrieve(ath, "//div[@class='sport_spotlight']//table//tr[8]//td[2]/text()")
    val occupation = retrieve(ath, "//div[@class='sport_spotlight']//table//tr[9]//td[2]/text()")

    val imageUrl = basePath+ath.getByXPath("//div[@class='sport_spotlight']//table//img[1]/@src").get(0).asInstanceOf[DomAttr].getValue()
    var sport = ""+ath.getByXPath("//div[@class='sport_spotlight']/text()[normalize-space(.)!='']").get(0)
    sport = sport.trim()

    val xml = <athlete>
                <name>{name}</name>
                <sport>{sport}</sport>
                <imageUrl>{imageUrl}</imageUrl>
                <birthDate>{date}</birthDate>
                <lives>{lives}</lives>
                <born>{born}</born>
                <country>{country}</country>
                <height>{height}</height>
                <weight>{weight}</weight>
                <club>{club}</club>
                <coach>{coach}</coach>
                <occupation>{occupation}</occupation>
              </athlete>;
    println(xml)
    xml
  }


  def retrieve(ath: HtmlPage, xpath : String) : String = {
    var list = ath.getByXPath(xpath)
    if (list.isEmpty) "" else "" + list.get(0)
  }

  def main (args: Array[String]) {
    downloadPage(basePage)
  }


}

