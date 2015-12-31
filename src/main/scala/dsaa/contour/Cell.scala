package dsaa.contour

object Cell {
  sealed trait Side
  case object LEFT extends Side
  case object RIGHT extends Side
  case object TOP extends Side
  case object BOTTOM extends Side
  case object NONE extends Side
}

/**
 * <p>A non-immutable class describing a Marching Squares Contour dsaa.contour.Cell.</p>
 */
class Cell {
  private var cellNdx: Byte = 0
  private final var flipped: Boolean = false
  private final var left: Double = 0.0
  private final var top: Double = 0.0
  private final var right: Double = 0.0
  private final var bottom: Double = 0.0

  private[contour] def this(ndx: Int, flipped: Boolean, left: Double, top: Double, right: Double, bottom: Double) {
    this()
    this.cellNdx = ndx.toByte
    this.flipped = if (flipped && ndx != 5 && ndx != 10) {
      println("dsaa.contour.Cell: Only saddle cells can be flipped. " +
        "Will set the 'flipped' flag to FALSE for this (" + ndx + ") cell's index")
      false
    }
    else {
      flipped
    }
    this.left = left
    this.top = top
    this.right = right
    this.bottom = bottom
  }

  /**
   * <p>Clear this cell's index.</p>
   *
   * <p>When building up shapes, it is possible to have disjoint regions and
   * holes in them. An easy way to build up a new shape from the cell's index
   * is to build sub-paths for one isoline at a time. As the shape is built
   * up, it is necessary to erase the (single) line afterward so that subsequent
   * searches for isolines will not loop indefinitely.
   */
  private[contour] def clear(): Unit = {
    cellNdx match {
      case 0 | 5 | 10 | 15 => ;
      case _ => cellNdx = 15
    }
  }

  /** @return this cell's algorithm index. */
  private[contour] def getCellNdx: Byte = cellNdx

  /**
   * @return crossing coordinates (already) normalized to [0.0..1.0].
   */
  private[contour] def getXY(edge: Cell.Side): Array[Double] = {
    import Cell._
    edge match {
      case BOTTOM => Array(bottom, 0.0F)
      case LEFT => Array(0.0F, left)
      case RIGHT => Array(1.0F, right)
      case TOP => Array(top, 1.0F)
      case _ => throw new IllegalStateException("getXY: N/A w/o a non-trivial edge")
    }
  }

  /** @return true if this dsaa.contour.Cell is a Saddle case. Returns false otherwise. */
  private[contour] def isSaddle: Boolean = cellNdx == 5 || cellNdx == 10


  /** @return true if this dsaa.contour.Cell is trivial; otherwise returns false. */
  private[contour] def isTrivial: Boolean = cellNdx == 0 || cellNdx == 15


  /** @return whether this cell is flipped or not. */
  private[contour] def isFlipped: Boolean = flipped

  override def toString: String = {
    new StringBuilder("dsaa.contour.Cell{index=").append(cellNdx)
      .append(", flipped? ").append(flipped)
      .append(", left=").append(left)
      .append(", top=").append(top)
      .append(", right=").append(right)
      .append(", bottom=").append(bottom)
      .append('}')
      .toString()
  }
}