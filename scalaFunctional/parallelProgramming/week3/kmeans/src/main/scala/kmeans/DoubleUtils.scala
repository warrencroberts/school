package kmeans

/**
  * Created by wrober003c on 5/31/17.
  */

object DoubleUtils {

  implicit class DoubleWithAlmostEquals(val d:Double) extends AnyVal {
    def ~=(d2:Double) = (d - d2).abs < 0.0000000001
  }

}

