package colman

import java.io.PrintWriter

/**
 * @author ${user.name}
 */
object App {
  def run(input: String, output: String, action: Action) {

    val s = io.Source.fromFile(input)
    val out = action(s.getLines().map(_.split("\\t").toSeq))

    val w = new PrintWriter(output)
    for(fs <- out) {
      w.println(fs.mkString("\t"))
    }
    w.flush()
    w.close()
    s.close()

  }

}
