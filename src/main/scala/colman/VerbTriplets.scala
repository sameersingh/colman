package colman

import java.io.File

import scala.collection.mutable

/**
 * Created by sameer on 5/14/15.
 */
object VerbTriplets {
  val baseDir = "/home/sameer/Downloads/gilette/"
  val inputFiles = (0 until 75).map(i => baseDir + "triplets-0517/part-r-%05d.gz".format(i))
  val triplets = baseDir + "triplets-0517/triplets.gz"
  val commonVerbs = baseDir + "triplets-0517/common-verbs.gz"
  val commonSubjects = baseDir + "triplets-0517/common-subjs.gz"
  val commonObjects = baseDir + "triplets-0517/common-objs.gz"
  val urls = baseDir + "triplets-0517/urls.gz"
  val urlVerbs = baseDir + "triplets-0517/verbs.gz"
  val urlObjectVerbs = baseDir + "triplets-0517/objs.gz"

  def run(action: Action, input: Seq[String], output: String) = {
    val as = Actions(action, new Writer(output, true))
    as(FileUtil.readFiles(input, true))
  }
}

object Lemmatize {
  def main(args: Array[String]): Unit = {
    def lemma(v: String) = {
      val split = v.split("\\|")
      val word = split.dropRight(1).mkString("|")
      val tag = split.last
      StanfordLemma.lemma(word, tag)
    }
    VerbTriplets.run(Actions(
      Transform(1, v => lemma(v)),
      Transform(2, v => lemma(v)),
      Transform(3, v => lemma(v))), VerbTriplets.inputFiles, VerbTriplets.triplets)
  }
}

object CommonVerbs {
  def main(args: Array[String]): Unit = {
    VerbTriplets.run(Actions(Cut(2, 4),
      SumLast(), Sort(1, true),
      Transform(1, _.toDouble.toInt.toString)),
      Seq(VerbTriplets.triplets),
      VerbTriplets.commonVerbs)
  }
}

object CommonSubjects {
  def main(args: Array[String]): Unit = {
    VerbTriplets.run(Actions(Cut(1, 4),
      SumLast(), Sort(1, true),
      Transform(1, _.toDouble.toInt.toString)),
      Seq(VerbTriplets.triplets),
      VerbTriplets.commonSubjects)
  }
}

object CommonObjects {
  def main(args: Array[String]): Unit = {
    VerbTriplets.run(Actions(Cut(3, 4),
      SumLast(), Sort(1, true),
      Transform(1, _.toDouble.toInt.toString)),
      Seq(VerbTriplets.triplets),
      VerbTriplets.commonObjects)
  }
}

object Urls{
  def main(args: Array[String]): Unit = {
    VerbTriplets.run(Actions(Cut(0, 4),
      SumLast(), Sort(1, true),
      Transform(1, _.toDouble.toInt.toString),
      new Filter(fs => UrlPolarity(fs(0)).isDefined),
      Transform(0, s => s + "\t" + UrlPolarity(s).get)
    ),
      Seq(VerbTriplets.triplets),
      VerbTriplets.urls)
  }
}

object Verbs {
  def main(args: Array[String]): Unit = {
    VerbTriplets.run(Actions(Cut(0, 2, 4),
      SumLast(), Sort(2, true),
      Transform(2, _.toDouble.toInt.toString)),
      Seq(VerbTriplets.triplets),
      VerbTriplets.urlVerbs)
  }
}

// Data for motivation table, object verb left/right count
object ObjectVerbs {
  def main(args: Array[String]): Unit = {
    VerbTriplets.run(Actions(
      new Filter(fs => UrlPolarity(fs(0)).isDefined),
      Transform(0, s => UrlPolarity(s).get),
      Cut(3, 2, 0, 4),
      SumLast(), new Filter(fs => fs(3).toDouble > 4.0),
      Sort(3, true), Sort(0, false),
      Transform(3, _.toDouble.toInt.toString)),
      Seq(VerbTriplets.triplets),
      VerbTriplets.urlObjectVerbs)
  }
}

// Data for motivation table, subject verb left/right count
object SubjectVerbs {
  def main(args: Array[String]): Unit = {
    VerbTriplets.run(Actions(
      new Filter(fs => UrlPolarity(fs(0)).isDefined),
      Transform(0, s => UrlPolarity(s).get),
      Cut(1, 2, 0, 4),
      SumLast(), new Filter(fs => fs(3).toDouble > 4.0),
      Sort(3, true), Sort(0, false),
      Transform(3, _.toDouble.toInt.toString)),
      Seq(VerbTriplets.triplets),
      VerbTriplets.urlObjectVerbs)
  }
}

// todo score each url/noun with a polarity (and count)
object NounPolarity {
  def main(args: Array[String]): Unit = {
    VerbTriplets.run(Actions(Cut(0, 2, 3, 4),
      SumLast(), Sort(3, true),
      Transform(3, _.toDouble.toInt.toString)),
      Seq(VerbTriplets.triplets),
      VerbTriplets.urlObjectVerbs)
  }
}

object VerbPolarity {
  case class Polarity(spSubj: Int, spObj: Int, subjObj: Int)

  val map = new mutable.HashMap[String, Polarity]

  def read(fname: String): Unit = {
    def str2int(s: String) = s match {
      case "N" => 0
      case "+" => 1
      case "-" => -1
    }
    map.clear()
    for(l <- FileUtil.read(fname, false, ",")) {
      assert(l.length == 4)
      assert(!map.contains(l(0)))
      map(l(0)) = Polarity(str2int(l(1)), str2int(l(2)), str2int(l(3)))
    }
  }

}

object UrlPolarity {
  val left = Map("Huffington Post" -> "huffingtonpost.com",
    "Slate" -> "slate.com",
    "Think Progress"->"thinkprogress.org",
    "Newsweek" -> "newsweek.com",
    "Politico" -> "politico.com",
    "Washington Post" -> "washingtonpost.com",
    "New York Times" -> "nytimes.com",
    "Los Angeles Times" -> "latimes.com",
    "The New Yorker" -> "newyorker.com",
    "Salon" -> "salon.com",
    "Guardian" -> "theguardian.com",
    "The Daily Beast" -> "thedailybeast.com",
    "OpEd News" -> "opednews.com",
    "RhReality Check"->"rhrealitycheck.org",
    "NPR" -> "npr.org")
  val right = Map("Fox News" -> "foxnews.com", // changed from org
    "New York Post" -> "nypost.com",
    "Washington Times" -> "washingtontimes.com",
    "The American Conservative" -> "theamericanconservative.com",
    "Weekly Standard" -> "weeklystandard.com",
    "National Review" -> "nationalreview.com",
    "Townhall"->"townhall.com",
    "Life Site News" -> "lifesitenews.com",
    "National Right To Life News" -> "nationalrighttolifenews.org",
    "Breitbart" -> "breitbart.com",
    "World Net Daily" -> "wnd.com",
    "City Journal" -> "city-journal.org",
    "The Hill" -> "thehill.com",
    "Human Events" -> "humanevents.com",
    "The Blaze" -> "theblaze.com")

  def apply(str: String): Option[String] = {
    if(left.exists(kv => str.contains(kv._2))) return Some("left")
    if(right.exists(kv => str.contains(kv._2))) return Some("right")
    None
  }
}