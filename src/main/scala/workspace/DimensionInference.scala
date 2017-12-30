package workspace

trait DimensionInference {
  def combine(other: DimensionInference, f: (Dimension, Dimension) => Dimension): DimensionInference = {
    flatCombine(other, (x: Dimension, y: Dimension) => ConcreteDimensionInference(f(x, y)): DimensionInference)
  }

  def flatCombine(other: DimensionInference, f: (Dimension, Dimension) => DimensionInference): DimensionInference = (this, other) match {
    case (BottomDimensionInference, _) => BottomDimensionInference
    case (_, BottomDimensionInference) => BottomDimensionInference
    case (TopDimensionInference, x) => x
    case (x, TopDimensionInference) => x
    case (ConcreteDimensionInference(dim1), ConcreteDimensionInference(dim2)) => f(dim1, dim2)
  }

  def combineWithEquals(other: DimensionInference): DimensionInference = {
    this.flatCombine(other,
      { case (dim1, dim2) => if (dim1 == dim2) ConcreteDimensionInference(dim1) else BottomDimensionInference })
  }

  def asTopOption: Option[Dimension] = this match {
    case TopDimensionInference => None
    case BottomDimensionInference => { throw new RuntimeException("934587234") }
    case ConcreteDimensionInference(dim) => Some(dim)
  }
}

case object TopDimensionInference extends DimensionInference
case object BottomDimensionInference extends DimensionInference
case class ConcreteDimensionInference(dim: Dimension) extends DimensionInference

object DimensionInference {
  def fromTopOption(mbDim: Option[Dimension]): DimensionInference = mbDim match {
    case None => TopDimensionInference
    case Some(x) => ConcreteDimensionInference(x)
  }
}
