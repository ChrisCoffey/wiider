package ccoffey.wiider

import com.gargoylesoftware.htmlunit.html.HtmlPage
import com.gargoylesoftware.htmlunit.{BrowserVersion, WebClient}
import org.openqa.selenium.htmlunit.HtmlUnitDriver
import scala.collection.JavaConversions._
import scala.collection.immutable.HashSet

object Main {
    type Href = String

    def main (args: Array[String]) {
        implicit val client = new WebClient(BrowserVersion.CHROME)
        client.getOptions.setCssEnabled(false)
        client.getOptions.setJavaScriptEnabled(false)

       // crawl("https://en.wikipedia.org/wiki/Decision_table" , "https://en.wikipedia.org/wiki/Flowchart")
        crawl("https://en.wikipedia.org/wiki/Decision_table" , "https://en.wikipedia.org/wiki/Diagram")
        .foreach(println)

    }

    def pageHrefs(page: HtmlPage): Seq[Href] =
        page.getAnchors.filter(_.getHrefAttribute.startsWith("/wiki/"))
        .map(href => s"https://en.wikipedia.org${href.getHrefAttribute}")
        .toList

    var visited = Map[Href, Href]()

    def crawl(start: Href, target: Href)(implicit client: WebClient): List[Href] = {

        val links = pageHrefs(client.getPage(start)).filterNot(visited.contains)
        links.foreach(l => cache(l, start))
        if(links.contains(target)) return start :: Nil

        var done = false
        var iterLinks = links
        var result: List[Href] = Nil
        while (!done) {
            println(s"There are ${iterLinks.length} links in this level")
            val res = crawlLevel(iterLinks, target)

            res match {
                case (Some(x), _) => {
                    done = true
                    result = unwindPath(x :: Nil)
                }
                case (None, Nil) => Nil
                case (None, ls) =>
                    println("next level")
                    iterLinks = ls
            }
        }

        result
    }

    def crawlLevel(level: Seq[Href], target: Href)(implicit client: WebClient): (Option[Href], List[Href]) = {
        level.foldLeft[(Option[Href], List[Href])]((None, Nil)){(acc, href) =>
            acc match {
                case (Some(_), _) => acc
                case (None, ls) =>
                    try{
                        val links = pageHrefs(client.getPage(href)).filterNot(visited.contains)
                        links.foreach(cache(_, href))
                        println(s"added ${links.length} to the cache")

                        if(links.contains(target))
                            (Some(href), Nil)
                        else
                            (None,  links.toList ::: ls)
                    } catch { case e: Exception =>
                        acc
                    }

            }
        }
    }

    var counter = 0
    private def cache(address: Href, parent: Href) = {
        counter += 1
        if(counter % 100 == 0) println(s"cached $counter links")
        visited += (address -> parent)
    }

    def unwindPath(start: List[Href]): List[Href] = {
         visited.get(start.head) match {
             case Some(addr) => unwindPath(addr :: start)
             case None => start
         }
    }

}
