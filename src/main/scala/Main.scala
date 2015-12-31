import java.awt.geom.{GeneralPath, AffineTransform}

import com.vividsolutions.jts.geom.Geometry
import dsaa.contour.Algorithm
import org.geotools.geometry.jts.JTS

import scala.util.{Success, Try}

/**
 * Created by ympons on 12/16/15.
 */
object Main {
  def main (args: Array[String]){
    val raster: Array[Array[Double]] = Array(
      Array(1,1,1,1,1),
      Array(1,2,3,2,1),
      Array(1,3,3,3,1),
      Array(1,2,3,2,1),
      Array(1,1,1,1,1)
    )
//    val random = new java.security.SecureRandom
//    val randomRaster = Array.fill[Double](1300,1300) { random.nextInt(5).toDouble }
//    for (i <- 0 until randomRaster.length) {
//      println("")
//      for (j <- 0 until randomRaster(0).length) {
//        print(s"${randomRaster(i)(j)} ")
//      }
//    }
    val levels = Array(1.5)//(1.0 to 10.0 by 1.0).toArray
    val paths = Algorithm.buildContours(raster, levels)

    val contours = paths map { path: GeneralPath =>
      val shape = path.createTransformedShape(new AffineTransform())
      Try(JTS.toGeometry(shape)) match {
        case Success(g) => g
        case _ => null
      }
    } filter { x => x != null }

    println(contours.length)
  }
}
