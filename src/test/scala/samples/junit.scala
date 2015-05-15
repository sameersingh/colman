package samples

import colman._
import org.junit._
import Assert._

import scala.collection.mutable.ArrayBuffer

@Test
class AppTest {

  def inputData: Iterator[Seq[String]] = {
    val numFields = 5
    val numRows = 100

    val data = new ArrayBuffer[Seq[String]]()
    for(i <- 0 until numRows) {
      data += (0 until numFields).map(j => (i/(j+1)).toString).toSeq
    }

    data.iterator
  }

  @Test
  def testOK() = {
    val actions = Actions(Cut(3,2), Uniq(), Cut(1, 2), SumLast(), ColMatch(0, "1.*"), Print(), Sink)

    actions(inputData).toSeq.view.force

    assert(true)
  }

}


