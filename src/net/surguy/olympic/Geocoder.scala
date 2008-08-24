package net.surguy.olympic

import com.gargoylesoftware.htmlunit.xml.XmlPage
import com.gargoylesoftware.htmlunit.html.HtmlPage
import com.gargoylesoftware.htmlunit.WebClient
import java.io.StringReader

/** Represent a latitude and longitude. */
case class Placemark(lat:String, long:String, name:String)

/** Use the Google Maps API to find a latitude and longitude for an address. */
class GoogleGeocoder(val apiKey:String) {

    val GOOGLE_URL = "http://maps.google.com/maps/geo?output=xml&key=%s&q=";
    val webClient = new WebClient()

    val NULL_PLACEMARK = new Placemark("","","Not found")
  
    val baseLookupUrl = String.format(GOOGLE_URL,Array(apiKey))

    def lookup(address:String) : Placemark = {
        // Too fast requests are rejected by Google
        Thread.sleep(250)
        // The majority of the addresses are in the UK - but Google will find "Birmingham, Alabama" for example
        // if the country is not specified.
        val fullAddress = if (address.contains(",")) address else address + ", UK"
        val lookupUrl = baseLookupUrl + java.net.URLEncoder.encode(fullAddress, "UTF-8")
        val page : XmlPage = webClient.getPage(lookupUrl).asInstanceOf[XmlPage]
        val coords = page.getByXPath("//*[name()='coordinates']/text()")
        if (coords.size==0) {
            println("Cannot find location for '" + address+"'");
            println(page.asXml)
            return NULL_PLACEMARK;
        } else {
            val latLong = coords.get(0).toString.split(",");
            return new Placemark(latLong(1), latLong(0), address);
        }
    }

}