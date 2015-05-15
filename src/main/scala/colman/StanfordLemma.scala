package colman

import edu.stanford.nlp.process.Morphology

/**
 * Created by sameer on 5/14/15.
 */
object StanfordLemma {

  val prep = Array("abroad", "across", "after", "ahead", "along", "aside", "away", "around", "back", "down", "forward", "in", "off", "on", "over", "out", "round", "together", "through", "up")
  val particles = prep.toList
  val morpha = new Morphology()

  def lemma(word: String, tag: String): String = {
    if (tag.length() > 0) {
      val pVerb = phrasalVerb(morpha, word, tag)
      if (pVerb == null) {
        morpha.lemma(word, tag)
      } else {
        pVerb
      }
    } else {
      morpha.stem(word)
    }
  }

  def phrasalVerb(morpha: Morphology, word: String, tag: String): String = {

    // must be a verb and contain an underscore
    assert(word != null)
    assert(tag != null)
    if(!tag.startsWith("VB")  || !word.contains("_")) return null

    // check whether the last part is a particle
    val verb = word.split("_")
    if(verb.length != 2) return null
    val particle = verb(1)
    if(particles.contains(particle)) {
      val base = verb(0)
      val lemma = morpha.lemma(base, tag)
      return lemma + '_' + particle
    }

    return null
  }

}
