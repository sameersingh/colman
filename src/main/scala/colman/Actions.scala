package colman

/**
 * Created by sameer on 5/14/15.
 */
trait Actions extends Action {
  def actions: Seq[Action]

  override def apply(data: Iterator[Seq[String]]): Iterator[Seq[String]] = {
    var input: Iterator[Seq[String]] = data
    var output: Iterator[Seq[String]] = Iterator.empty
    for(a <- actions) {
      output = a.apply(input)
      input = output
    }
    output
  }
}

object Actions {
  def apply(acts: Action*): Actions = {
    new Actions {
      override def actions: Seq[Action] = acts
    }
  }
}