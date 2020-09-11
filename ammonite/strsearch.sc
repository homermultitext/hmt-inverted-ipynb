
// Check for most recent release at
// https://github.com/homermultitext/hmt-archive/tree/master/releases-cex
// and change this value if needed:
val releaseId = "2020i"

// 1. Add maven repository where we can find our libraries
val myBT = coursierapi.MavenRepository.of("https://dl.bintray.com/neelsmith/maven")
interp.repositories() ++= Seq(myBT)

// 2. Make libraries available with `$ivy` imports:
import $ivy.`edu.holycross.shot::scm:7.4.0`
import $ivy.`edu.holycross.shot::ohco2:10.20.4`
import $ivy.`edu.holycross.shot.cite::xcite:4.3.0`
import $ivy.`edu.holycross.shot::dse:7.1.3`

import edu.holycross.shot.ohco2._
import edu.holycross.shot.scm._
import edu.holycross.shot.cite._
import edu.holycross.shot.dse._

val xmlUrl = s"https://raw.githubusercontent.com/homermultitext/hmt-archive/master/releases-cex/hmt-${releaseId}-scholia-xml.cex"
val scholiaXml = CorpusSource.fromUrl(xmlUrl, cexHeader=false)

import scala.xml._

val pnIndex = for (n <- scholiaXml.nodes) yield {
  val x = XML.loadString(n.text)
  val pns = x \\ "persName"
  pns.toVector.map(pn => {
    val attValue = pn.attributes.asAttrMap.getOrElse("n", "No @n attribute on " + pn.text)
    n.urn -> attValue
  })
}
val personMap = pnIndex.flatten.groupBy(_._2)

import edu.holycross.shot.scm._
val url = s"https://raw.githubusercontent.com/homermultitext/hmt-archive/master/releases-cex/hmt-${releaseId}.cex"
val lib = CiteLibrarySource.fromUrl(url)
val corpus = lib.textRepository.get.corpus
val dsev = DseVector.fromCiteLibrary(lib)
val scholia = corpus.nodes.filter(_.urn.textGroup == "tlg5026")

val pageBaseUrl = "http://www.homermultitext.org/facsimiles/venetus-a/"

def search(s: String) = {
  val matchedPsgs = scholia.filter(_.text.contains(s))
  val pls = if (matchedPsgs.size == 1) { "" } else  { "s" }
  val hdr = s"<h2>Search for string ${s}</h2>" +
  s"<p>Found ${matchedPsgs.size} passage${pls}</p>"
  val results = for ( (urn, idx)  <- matchedPsgs.map(_.urn).zipWithIndex) yield {
    val scholion = urn.collapsePassageBy(1)
    //println(scholion)
    val nd = corpus.nodes.filter(nd => scholion > nd.urn)
    //println(nd)
    val text = nd.map(n => "<blockquote>" + n.text.replaceAll(s, "<strong>" + s + "</strong>") + "</blockquote>" )
    val pgOpt = dsev.tbsForText(scholion)
    pgOpt match  {
      case None => {

        s"<li> <strong>${idx + 1}/${matchedPsgs.size}</strong> ${scholion} (Sadly, no page indexed in DSE) "  + text.mkString("\n")  + "</li>"
      }
      case _ => {
        val pg = pgOpt.get.objectComponent
        val url = pageBaseUrl + pg + "/"

        val link = "<a href=\"" + url + "\">facsimile</a>"

        s"<li> <strong>${idx + 1}/${matchedPsgs.size}</strong> ${scholion}, page ${pg} (${link})" + text.mkString("\n") + "</li>"
      }
    }
  }
  Html(hdr + results.mkString("\n"))
}

search("κονομ")
