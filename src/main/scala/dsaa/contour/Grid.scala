package dsaa.contour

/**
 * <p>Given a two-dimensional scalar field (rectangular array of individual
 * numerical values) the Marching Squares algorithm applies a <em>threshold</em>
 * (a.k.a contour level or isovalue) to make a binary image containing:</p>
 *
 * <ul>
 * <li>1 where the data value is above the isovalue,</li>
 * <li>0 where the data value is below the isovalue.</li>
 * </ul>
 *
 * <p>Every 2x2 block of pixels in the binary image forms a contouring cell, so
 * the whole image is represented by a grid of such cells (shown in green in
 * the picture below). Note that this contouring grid is one cell smaller in
 * each direction than the original 2D data field.</p>
 */
class Grid {
  private[contour] final var cells: Array[Array[Cell]] = null
  private[contour] final var rowCount: Int = 0
  private[contour] final var colCount: Int = 0
  private[contour] final var threshold: Double = .0
  @transient
  private var str: String = null

  private[contour] def this(cells: Array[Array[Cell]], threshold: Double) {
    this()
    this.cells = cells
    rowCount = cells.length
    colCount = cells(0).length
    this.threshold = threshold
  }

  private[contour] def getCellAt(r: Int, c: Int): Cell = cells(r)(c)

  private[contour] def getCellNdxAt(r: Int, c: Int): Int = {
    cells(r)(c) match {
      case cell: Cell if cell != null => cell.getCellNdx
      case _ => 0
    }
  }

  override def toString: String = {
    if (str == null) {
      str = new StringBuilder("dsaa.contour.Grid{rowCount=").append(rowCount)
        .append(", colCount=").append(colCount)
        .append(", threshold=").append(threshold)
        .append('}')
        .toString()
    }
    str
  }
}
