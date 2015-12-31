package dsaa.contour

import java.awt.geom._

/**
 * <p>An object that knows how to translate a dsaa.contour.Grid of Marching Squares Contour
 * Cells into a Java AWT General Path.</p>
 */
object PathGenerator {
  private val EPSILON: Double = 1E-7

  def apply(grid: Grid): GeneralPath = {
    new PathGenerator().generate(grid)
  }
}

class PathGenerator {
  /**
   * <p>Construct a GeneralPath representing the isoline, itself represented
   * by a given dsaa.contour.Grid.</p>
   *
   * <p><b>IMPLEMENTATION NOTE:</b> This method is destructive. It alters
   * the dsaa.contour.Grid instance as it generates the resulting path. If the 'original'
   * dsaa.contour.Grid instance is needed after invoking this method then it's the
   * responsibility of the caller to deep clone it before passing it here.</p>
   *
   * @param grid the matrix of contour cells w/ side crossing coordinates
   *             already interpolated and normalized; i.e. in the range 0.0..1.0.
   * @return the geometries of a contour, including sub-path(s) for disjoint
   *         areas and holes.
   */
  private[contour] def generate(grid: Grid): GeneralPath = {
    val result = new GeneralPath(Path2D.WIND_EVEN_ODD)
    for (r <- 0 until grid.rowCount)
      for (c <- 0 until grid.colCount) {
        val cell: Cell = grid.getCellAt(r, c)
        if (cell != null && !cell.isTrivial && !cell.isSaddle) {
          update(grid, r, c, result)
        }
      }
    result
  }

  /**
   * <p>Return the first side that should be used in a CCW traversal.</p>
   *
   * @param cell the dsaa.contour.Cell to process.
   * @param prev previous side, only used for saddle cells.
   * @return the 1st side of the line segment of the designated cell.
   */
  private def firstSide(cell: Cell, prev: Cell.Side): Cell.Side = {
    import Cell._
    cell.getCellNdx match {
      case 1 | 3 | 7 => Cell.LEFT
      case 2 | 6 | 14 => Cell.BOTTOM
      case 4 | 11 | 12 | 13 => Cell.RIGHT
      case 8 | 9 => Cell.TOP
      case 5 =>
        prev match {
          case LEFT => Cell.RIGHT
          case RIGHT => Cell.LEFT
          case _ =>
            val m: String = "Saddle w/ no connected neighbour; dsaa.contour.Cell = " + cell + ", previous side = " + prev
            println("firstSide: " + m + ". Throw ISE")
            throw new IllegalStateException(m)
        }
      case 10 =>
        prev match {
          case BOTTOM => Cell.TOP
          case TOP => Cell.BOTTOM
          case _ =>
            val m: String = "Saddle w/ no connected neighbour; dsaa.contour.Cell = " + cell + ", previous side = " + prev
            println("firstSide: " + m + ". Throw ISE")
            throw new IllegalStateException(m)
        }
      case _ =>
        val m: String = "Attempt to use a trivial cell as a start node: " + cell
        println("firstSide: " + m + ". Throw ISE")
        throw new IllegalStateException(m)
    }
  }

  /**
   * <p>Find the side on which lies the next cell to use in a CCW traversal.</p>
   *
   * @param cell the dsaa.contour.Cell to process.
   * @param prev previous side, only used for saddle cells.
   * @return side where the next cell is to be picked.
   */
  private def nextSide(cell: Cell, prev: Cell.Side): Cell.Side = secondSide(cell, prev)

  /**
   * <p>Return the second side that should be used in a CCW traversal.</p>
   *
   * @param cell the dsaa.contour.Cell to process.
   * @param prev previous side, only used for saddle cells.
   * @return the 2nd side of the line segment of the designated cell.
   */
  private def secondSide(cell: Cell, prev: Cell.Side): Cell.Side = {
    import Cell._
    cell.getCellNdx match {
      case 8 | 12 | 14 => Cell.LEFT
      case 1 | 9 | 13 => Cell.BOTTOM
      case 2 | 3 | 11 => Cell.RIGHT
      case 4 | 6 | 7 => Cell.TOP
      case 5 =>
        prev match {
          case LEFT => if (cell.isFlipped) Cell.BOTTOM else Cell.TOP
          case RIGHT => if (cell.isFlipped) Cell.TOP else Cell.BOTTOM
          case _ =>
            val m: String = "Saddle w/ no connected neighbour; dsaa.contour.Cell = " + cell + ", previous side = " + prev
            println("secondSide: " + m + ". Throw ISE")
            throw new IllegalStateException(m)
        }
      case 10 =>
        prev match {
          case BOTTOM => if (cell.isFlipped) Cell.RIGHT else Cell.LEFT
          case TOP => if (cell.isFlipped) Cell.LEFT else Cell.RIGHT
          case _ =>
            val m: String = "Saddle w/ no connected neighbour; dsaa.contour.Cell = " + cell + ", previous side = " + prev
            println("secondSide: " + m + ". Throw ISE")
            throw new IllegalStateException(m)
        }
      case _ =>
        val m: String = "Attempt to use a trivial dsaa.contour.Cell as a node: " + cell
        println("secondSide: " + m + ". Throw ISE")
        throw new IllegalStateException(m)
    }
  }

  /**
   * <p>A given contour can be made up of multiple disconnected regions, each
   * potentially having multiple holes. Both regions and holes are captured as
   * individual sub-paths.</p>
   *
   * <p>The process is iterative. It starts w/ an empty GeneralPath instance
   * and continues until all Cells are processed. With every invocation the
   * GeneralPath object is updated to reflect the new sub-path(s).</p>
   *
   * <p>Once a non-saddle cell is used it is cleared so as to ensure it will
   * not be re-used when finding sub-paths w/in the original path.</p>
   *
   * @param grid on input the matrix of cells representing a given contour.
   *             Note that the process will alter the Cells, so on output the original
   *             dsaa.contour.Grid instance _will_ be modified. In other words this method is NOT
   *             idempotent when using the same object references and values.
   * @param row row index of the start dsaa.contour.Cell.
   * @param col column index of the start dsaa.contour.Cell.
   * @param path a non-null GeneralPath instance to update.
   */
  private def update(grid: Grid, row: Int, col: Int, path: GeneralPath): Unit = {
    import Cell._
    var r = row
    var c = col

    var prevSide: Cell.Side = Cell.NONE
    val start: Cell = grid.getCellAt(r, c)
    var pt: Array[Double] = start.getXY(firstSide(start, prevSide))
    var x: Double = c + pt(0)
    var y: Double = r + pt(1)
    path.moveTo(x, y)

    pt = start.getXY(secondSide(start, prevSide))
    var xPrev: Double = c + pt(0)
    var yPrev: Double = r + pt(1)

    prevSide = nextSide(start, prevSide)
    prevSide match {
      case BOTTOM => r -= 1
      case LEFT => c -= 1
      case RIGHT => c += 1
      case TOP => r += 1
      case _ =>
    }
    start.clear()

    var currentCell: Cell = grid.getCellAt(r, c)
    while (start ne currentCell) {
      pt = currentCell.getXY(secondSide(currentCell, prevSide))
      x = c + pt(0)
      y = r + pt(1)
      if (Math.abs(x - xPrev) > PathGenerator.EPSILON && Math.abs(y - yPrev) > PathGenerator.EPSILON) {
        path.lineTo(x, y)
      }
      xPrev = x
      yPrev = y
      prevSide = nextSide(currentCell, prevSide)
      prevSide match {
        case BOTTOM => r -= 1
        case LEFT => c -= 1
        case RIGHT => c += 1
        case TOP => r += 1
        case _ =>
          println("update: Potential loop! Current cell = " + currentCell + ", previous side = " + prevSide)
      }
      currentCell.clear()
      currentCell = grid.getCellAt(r, c)
    }
    path.closePath()
  }
}
