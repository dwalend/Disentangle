package net.walend.disentangle.graph.semiring.benchmark

import java.io.{File, FileOutputStream, PrintStream}

/**
 *
 *
 * @author dwalend
 * @since v0.1.2
 */
object TimingStudy {

  def timeFunction[T](body: => T):(T,Long) = {
    val startTime:Long = System.nanoTime()
    val result = body
    val endTime:Long = System.nanoTime()
    (result,endTime-startTime)
  }

  def timeToStdOut[T](name:String)(body: => T) = {
    val (result,time) = timeFunction(body)

    println(s"$name,${time/1000000000}")

    result
  }
}

/**
 * @param timeF Runs the timing study.
 * @param expectedF Predicted time based on early, calibration results
 * @param minExponent Use 2^^minExponent nodes as the smallest graph in the study
 * @param maxExponent Use 2^^maxExponent nodes as the largest graph in the study
 * @param outFile File to write output, will use std out for None
 */
case class TimingStudy(timeF:Int => Long,expectedF:((Int,Long),Int) => Long,minExponent:Int,maxExponent:Int,outFile:Option[File]) {

  def study():Seq[(Int,Long,Long)] = {

    warmUp(64,{timeF(32)})
    warmUp(64,{timeF(64)})
    warmUp(64,{timeF(128)})

    writeHeader()

    val nodeCounts: Seq[Int] = nodeCountSeq(minExponent,maxExponent)

    val headNodeCount = nodeCounts.head
    val calibration: (Int, Long) = (headNodeCount,timeF(headNodeCount))
    val headExpected = expectedF(calibration,headNodeCount)
    writeResult(calibration._1,calibration._2,headExpected)

    nodeCounts.tail.map(x => writeResult(x,timeF(x),expectedF(calibration,x)))
  }

  def nodeCountSeq(minExponent:Int,maxExponent:Int):Seq[Int] = {
    val exponents: Seq[Double] = (0 to (maxExponent - minExponent) * 4).map(_.toDouble).map(_ / 4 + minExponent)

    exponents.map(x => Math.pow(2, x).toInt)
  }

  def warmUp[T](number:Int,body: => T) = {
    for(i <- 0 until number) body
  }

  def writeHeader():Unit = {
    val header:String = "nodes,measured,expected"
    withOut(false) { out =>
      out.println(header)
    }
  }

  def writeResult(nodeCount:Int,result:Long,expected:Long):(Int,Long,Long) = {
    withOut(true) { out =>
      out.println(s"${nodeCount},${result},${expected}")
    }
    (nodeCount,result,expected)
  }

  def withOut[T](append:Boolean)(block:(PrintStream => T)): T = {
    val out = outFile.fold(System.out)(file => new PrintStream(new FileOutputStream(file,append)))
    val result = block(out)
    if(out != System.out) out.close()
    result
  }
}

trait Timeable {

  def measureTime(nodeCount:Int):Long

  def expectedTime(calibration:(Int,Long),nodeCount:Int):Long
}