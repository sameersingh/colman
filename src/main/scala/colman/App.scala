package colman

import java.io.PrintWriter

/**
 * @author ${user.name}
 */
object App {
  def run(input: String, output: String, action: Action) {

    val out = action(FileUtil.read(input, false))
    val w = new PrintWriter(output)
    for(fs <- out) {
      w.println(fs.mkString("\t"))
    }
    w.flush()
    w.close()
  }

}
