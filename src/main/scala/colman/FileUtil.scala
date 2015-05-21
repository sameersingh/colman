package colman

import java.io._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

import scala.io.BufferedSource

/**
 * Created by sameer on 5/18/15.
 */
object FileUtil {
  def inputSource(fname: String, gzip: Boolean): BufferedSource = {
    io.Source.fromInputStream(
      if (gzip) new GZIPInputStream(new FileInputStream(fname))
      else new FileInputStream(fname))("UTF-8")
  }

  def read(fname: String, gzip: Boolean, delim: String = "\\t"): Iterator[Seq[String]] = {
    val source = inputSource(fname, gzip)
    source.getLines().map(_.split(delim).toSeq)
  }

  def readFiles(fnames: Seq[String], gzip: Boolean, delim: String = "\\t"): Iterator[Seq[String]] =
    fnames.iterator.map(fname => inputSource(fname, gzip)).flatMap(_.getLines().map(_.split(delim).toSeq))

  def writer(fname: String, gzip: Boolean): PrintWriter =
    new PrintWriter(
      if (gzip) new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(fname)), "UTF-8")
      else new FileWriter(fname))
}
