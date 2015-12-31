package dsaa.contour

import java.awt.geom.GeneralPath

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Success

/**
 * <p>Implementation of the Marching Squares algorithm described in:
 * {@code https://en.wikipedia.org/wiki/Marching_squares}</p>
 */
object Algorithm {

  def buildContours(data: Array[Array[Double]], levels: Array[Double]): Array[GeneralPath] = {
    var t: Long = -System.currentTimeMillis

    var max: Double = data(0)(0)
    val min: Double = data.flatten reduceLeft((x, y) => {
      val r = x min y
      max = max max y
      r
    })

    val result: Array[GeneralPath] = if (min != max) generate(pad(data, min - 1), levels) else {
      val m: String = "All values are equal. Cannot build contours for a constant field"
      println(": " + m + ". Throw IAE")
      throw new IllegalArgumentException(m)
    }
    t += System.currentTimeMillis
    println("*** Built " + levels.length + " contours in " + t + " ms.")
    result
  }

  private def generate(data: Array[Array[Double]], isovalues: Array[Double]): Array[GeneralPath] = {
    implicit val ec = ExecutionContext.global

    def contourFuture(i: Int, isovalue: Double) = Future {
      (i, PathGenerator(contour(data, isovalue)))
    }
    val futures = Future.traverse(isovalues.indices.toList)(i => contourFuture(i, isovalues(i)))
    Await.result(futures, 5 minutes)
    val result = new Array[GeneralPath](isovalues.length)
    futures.value match {
      case Some(Success(f)) => f foreach { v => result(v._1) = v._2 }
      case _ => ;
    }
    result
  }

  private[contour] def contour(data: Array[Array[Double]], isovalue: Double): Grid = {
    val rowCount: Int = data.length
    val colCount: Int = data(0).length

    // Every 2x2 block of pixels in the binary image forms a contouring cell,
    // so the whole image is represented by a grid of such cells. Note that
    // this contouring grid is one cell smaller in each direction than the
    // original 2D field.
    val cells: Array[Array[Cell]] = Array.ofDim[Cell](rowCount - 1, colCount - 1)
    for (r <- 0 until rowCount - 1)
      for (c <- 0 until colCount - 1) {
        // Compose the 4 bits at the corners of the cell to build a binary
        // index: walk around the cell in a clockwise direction appending
        // the bit to the index, using bitwise OR and left-shift, from most
        // significant bit at the top left, to least significant bit at the
        // bottom left.  The resulting 4-bit index can have 16 possible
        // values in the range 0-15.
        var ndx: Int = 0
        val tl: Double = data(r + 1)(c)
        val tr: Double = data(r + 1)(c + 1)
        val br: Double = data(r)(c + 1)
        val bl: Double = data(r)(c)
        ndx |= (if (tl > isovalue) 0 else 8)
        ndx |= (if (tr > isovalue) 0 else 4)
        ndx |= (if (br > isovalue) 0 else 2)
        ndx |= (if (bl > isovalue) 0 else 1)
        var flipped: Boolean = false
        if (ndx == 5 || ndx == 10) {
          // resolve the ambiguity by using the average data value for the
          // center of the cell to choose between different connections of
          // the interpolated points.
          val center: Double = (tl + tr + br + bl) / 4
          if (ndx == 5 && center < isovalue) {
            flipped = true
          }
          else if (ndx == 10 && center < isovalue) {
            flipped = true
          }
        }
        // NOTE (rsn) - we only populate the grid w/ non-trivial cells;
        // i.e. those w/ an index different than 0 and 15.
        if (ndx != 0 && ndx != 15) {
          // Apply linear interpolation between the original field data
          // values to find the exact position of the contour line along
          // the edges of the cell.
          var left: Double = 0.5F
          var top: Double = 0.5F
          var right: Double = 0.5F
          var bottom: Double = 0.5F
          ndx match {
            case 1 | 14 =>
              left = ((isovalue - bl) / (tl - bl)).toFloat
              bottom = ((isovalue - bl) / (br - bl)).toFloat
            case 2 | 13 =>
              bottom = ((isovalue - bl) / (br - bl)).toFloat
              right = ((isovalue - br) / (tr - br)).toFloat
            case 3 | 12 =>
              left = ((isovalue - bl) / (tl - bl)).toFloat
              right = ((isovalue - br) / (tr - br)).toFloat
            case 4 =>
              top = ((isovalue - tl) / (tr - tl)).toFloat
              right = ((isovalue - br) / (tr - br)).toFloat
            case 5 =>
              left = ((isovalue - bl) / (tl - bl)).toFloat
              bottom = ((isovalue - bl) / (br - bl)).toFloat
              top = ((isovalue - tl) / (tr - tl)).toFloat
              right = ((isovalue - br) / (tr - br)).toFloat
            case 6 | 9 =>
              bottom = ((isovalue - bl) / (br - bl)).toFloat
              top = ((isovalue - tl) / (tr - tl)).toFloat
            case 7 | 8 =>
              left = ((isovalue - bl) / (tl - bl)).toFloat
              top = ((isovalue - tl) / (tr - tl)).toFloat
            case 10 =>
              left = ((isovalue - bl) / (tl - bl)).toFloat
              bottom = ((isovalue - bl) / (br - bl)).toFloat
              top = ((isovalue - tl) / (tr - tl)).toFloat
              right = ((isovalue - br) / (tr - br)).toFloat
            case 11 =>
              top = ((isovalue - tl) / (tr - tl)).toFloat
              right = ((isovalue - br) / (tr - br)).toFloat
            case _ =>
              val m: String = "Unexpected cell index " + ndx
              throw new IllegalStateException(m)
          }
          cells(r)(c) = new Cell(ndx, flipped, left, top, right, bottom)
        }
      }

    new Grid(cells, isovalue)
  }

  /**
   * <p>Pad data with a given 'guard' value.</p>
   *
   * @param data matrix to pad.
   * @param guard the value to use for padding. It's expected to be less than
   *              the minimum of all data cell values.
   * @return the resulting padded matrix which will be larger by 2 in both
   *         directions.
   */
  private def pad(data: Array[Array[Double]], guard: Double): Array[Array[Double]] = {
    val rowCount: Int = data.length
    val colCount: Int = data(0).length
    val result: Array[Array[Double]] = Array.ofDim[Double](rowCount + 2, colCount + 2)

    // top and bottom rows
    for (j <- 0 until colCount + 2) {
      result(0)(j) = guard
      result(rowCount + 1)(j) = guard
    }

    // left- and right-most columns excl. top and bottom rows
    for (i <- 1 until rowCount + 1) {
      result(i)(0) = guard
      result(i)(colCount + 1) = guard
    }

    // the middle
    for (i <- 0 until rowCount) {
      Array.copy(data(i), 0, result(i + 1), 1, colCount)
    }

    result
  }

}