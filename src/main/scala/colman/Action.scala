package colman

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

/**
 * Created by sameer on 5/14/15.
 */
trait Action extends (Iterator[Seq[String]] => Iterator[Seq[String]])

object Func {
  def apply(f: Iterator[Seq[String]] => Iterator[Seq[String]]) = new Action {
    override def apply(data: Iterator[Seq[String]]): Iterator[Seq[String]] = f(data)
  }
}

trait Emit extends Action {
  override def apply(data: Iterator[Seq[String]]): Iterator[Seq[String]] = data.flatMap(d => emit(d))

  def emit(fs: Seq[String]): Seq[Seq[String]]
}

trait PerRow extends Action {

  override def apply(data: Iterator[Seq[String]]): Iterator[Seq[String]] = data.flatMap(d => apply(d).iterator)

  def apply(fields: Seq[String]): Option[Seq[String]]
}

object AssertLength {
  def apply(len: Int) = new PerRow {
    override def apply(fields: Seq[String]): Option[Seq[String]] = {
      assert(fields.length == len, s"Length of ${fields.mkString(", ")} is ${fields.length} != $len" )
      Some(fields)
    }
  }
}

class Cut(output: Seq[Int]) extends PerRow {
  override def apply(fields: Seq[String]): Option[Seq[String]] = Some(output.map(i => fields(i)))
}

object Cut {
  def apply(output: Int*) = new Cut(output)
}

class Print(prefix: String = "") extends PerRow {
  override def apply(fields: Seq[String]): Option[Seq[String]] = {
    println(prefix + fields.mkString("\t"))
    Some(fields)
  }
}

object Print {
  def apply(prefix: String = "") = new Print(prefix)
}

object Sink extends PerRow {
  override def apply(fields: Seq[String]): Option[Seq[String]] = None
}

class Filter(pred: Seq[String] => Boolean) extends PerRow {
  override def apply(fields: Seq[String]): Option[Seq[String]] = if(pred(fields)) Some(fields) else None
}

class ColMatch(key: Int, regex: Regex) extends Filter(f => !regex.findAllIn(f(key)).isEmpty)

object ColMatch {
  def apply(key: Int = 0, pattern: String) = new ColMatch(key, pattern.r)
}

/**
  * Transform a single column, leaving the rest unchanged
  * @param key the column number to modify
  * @param f the function to apply to the column members
  */
class Transform(key: Int, f: String => String) extends PerRow {
  override def apply(fields: Seq[String]): Option[Seq[String]] = Some(fields.slice(0, key) ++ Seq(f(fields(key))) ++ fields.slice(key + 1, fields.length))
}

object Transform {
  def apply(key: Int = 0, f: String => String) = new Transform(key, f)

  def all(t: Seq[String] => Option[Seq[String]]) = new PerRow {
    override def apply(fields: Seq[String]): Option[Seq[String]] = t(fields)
  }
}

trait Aggregate extends Action {

  override def apply(data: Iterator[Seq[String]]): Iterator[Seq[String]] = {
    for(d <- data) apply(d)
    output
  }

  def apply(fields: Seq[String]): Unit

  def output: Iterator[Seq[String]]
}

class Writer(fname: String, gzip: Boolean) extends Aggregate {
  val writer = FileUtil.writer(fname, gzip)
  var idx = 0l
  override def apply(fields: Seq[String]): Unit = {
    writer.println(fields.mkString("\t"))
    idx += 1l
    if(idx % 10000l == 0) print(".")
    if(idx % 1000000l == 0) println(": " + idx)
  }

  override def output: Iterator[Seq[String]] = {
    writer.flush()
    writer.close()
    Iterator.empty
  }
}

class Group(key: Int) extends Aggregate {

  val map = new mutable.LinkedHashMap[String, ArrayBuffer[Seq[String]]]

  override def apply(fields: Seq[String]): Unit = {
    val arr = map.getOrElseUpdate(fields(key), new ArrayBuffer)
    arr += fields
  }

  override def output: Iterator[Seq[String]] = {
    map.valuesIterator.flatMap(_.iterator)
  }
}

object Group {
  def apply(key: Int = 0) = new Group(key)
}

class Sort(key: Int, numeric: Boolean) extends Group(key) {

  override def output: Iterator[Seq[String]] = {
    val keys = if(numeric) map.keys.toSeq.sortBy(-_.toDouble) else map.keys.toSeq.sorted
    keys.iterator.flatMap(k => map(k))
  }
}

object Sort {
  def apply(key: Int = 0, numeric: Boolean = false) = new Sort(key, numeric)
}

class Uniq(count: Boolean = true) extends Aggregate {

  val map = new mutable.LinkedHashMap[Seq[String], Int]

  override def apply(fields: Seq[String]): Unit = {
    map(fields) = map.getOrElse(fields, 0) + 1
  }

  override def output: Iterator[Seq[String]] = {
    if(count) map.iterator.map(p => p._1 ++ Seq(p._2.toString))
    else map.keysIterator
  }
}

object Uniq {
  def apply(count: Boolean = true) = new Uniq(count)
}

class SumLast(n: Int = 1) extends Aggregate {

  val map = new mutable.LinkedHashMap[Seq[String], Seq[Double]]

  override def apply(fields: Seq[String]): Unit = {
    val key = fields.dropRight(n)
    val count = fields.takeRight(n).map(_.toDouble)
    map(key) = map.getOrElse(key, Seq.fill(n)(0.0)).zip(count).map(sc => sc._1 + sc._2)
  }

  override def output: Iterator[Seq[String]] = {
    map.iterator.map(p => p._1 ++ p._2.map(_.toString))
  }
}

object SumLast {
  def apply(n: Int = 1) = new SumLast(n)
}
