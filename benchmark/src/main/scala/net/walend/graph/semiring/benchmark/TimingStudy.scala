package net.walend.graph.semiring.benchmark

/**
 *
 *
 * @author dwalend
 * @since v0.1.2
 */
object TimingStudy {

  def study(maxExponent:Int,timeF:Int => Long,expectedF:((Int,Long),Int) => Long):Seq[(Int,Long,Long,Double)] = {

    warmUp(16,{timeF(32)})
    warmUp(16,{timeF(64)})
    warmUp(16,{timeF(128)})
    val nodeCountAndTime:Seq[(Int,Long)] = nodeCountsFrom32(maxExponent).map(x=>(x,timeF(x)))

    val calibration = nodeCountAndTime.head
    val expected = nodeCountAndTime.map(x => x._1 -> expectedF(calibration,x._1)).toMap
    val ratio = nodeCountAndTime.map(x => x._1 -> x._2.toDouble/expected.get(x._1).get).toMap

    nodeCountAndTime.map(x => (x._1,x._2,expected(x._1),ratio(x._1)))
  }

  def nodeCountsFrom32(exponent:Int):Seq[Int] = {
    (5.0.to(exponent.toDouble,0.25)).map(x => Math.pow(2,x).toInt)
  }

  def warmUp[T](number:Int,body: ⇒ T) = {
    for(i <- 0 until number) body
  }

  def timeFunction[T](body: ⇒ T):(T,Long) = {
    val startTime:Long = System.nanoTime()
    val result = body
    val endTime:Long = System.nanoTime()
    (result,endTime-startTime)
  }

}
