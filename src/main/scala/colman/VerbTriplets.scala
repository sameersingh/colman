package colman

import java.io.File

/**
 * Created by sameer on 5/14/15.
 */
object VerbTriplets {
  val input = "/home/sameer/Downloads/gilette/triplets-test"

  def run(action: Action, outputSuffix: String) = {
    App.run(input, new File(input).getAbsolutePath + "." + outputSuffix, action)
  }
}

object Verbs {
  def main(args: Array[String]): Unit = {
    VerbTriplets.run(Actions(Cut(1, 3),
      Transform(0, v => StanfordLemma.lemma(v, "VB")),
      SumLast(), Sort(1, true),
      Transform(1, _.toDouble.toInt.toString)), "verbs")
  }
}

object VerbSubject {
  def main(args: Array[String]): Unit = {
    VerbTriplets.run(Actions(Cut(0, 1, 3),
      Transform(0, v => StanfordLemma.lemma(v, "NN")),
      Transform(1, v => StanfordLemma.lemma(v, "VB")),
      SumLast(), Sort(2, true),
      Transform(2, _.toDouble.toInt.toString)), "subj")
  }
}

object VerbObject {
  def main(args: Array[String]): Unit = {
    VerbTriplets.run(Actions(Cut(1, 2, 3),
      Transform(0, v => StanfordLemma.lemma(v, "VB")),
      Transform(1, v => StanfordLemma.lemma(v, "NN")),
      SumLast(), Sort(2, true),
      Transform(2, _.toDouble.toInt.toString)), "obj")
  }
}