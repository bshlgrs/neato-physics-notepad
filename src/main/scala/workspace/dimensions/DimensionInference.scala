package workspace.dimensions

sealed trait DimensionInference {
  def combine(other: DimensionInference, f: (SiDimension, SiDimension) => SiDimension): DimensionInference = {
    flatCombine(other, (x: SiDimension, y: SiDimension) => ConcreteDimensionInference(f(x, y)): DimensionInference)
  }

  def flatCombine(other: DimensionInference, f: (SiDimension, SiDimension) => DimensionInference): DimensionInference = (this, other) match {
    case (BottomDimensionInference, _) => BottomDimensionInference
    case (_, BottomDimensionInference) => BottomDimensionInference
    case (TopDimensionInference, x) => TopDimensionInference
    case (x, TopDimensionInference) => TopDimensionInference
    case (ConcreteDimensionInference(dim1), ConcreteDimensionInference(dim2)) => f(dim1, dim2)
  }

  def combineWithEquals(other: DimensionInference): DimensionInference = (this, other) match {
    case (BottomDimensionInference, _) => BottomDimensionInference
    case (_, BottomDimensionInference) => BottomDimensionInference
    case (TopDimensionInference, x) => TopDimensionInference
    case (x, TopDimensionInference) => TopDimensionInference
    case (ConcreteDimensionInference(dim1), ConcreteDimensionInference(dim2)) => {
      if (dim1 == dim2) ConcreteDimensionInference(dim1)
      else BottomDimensionInference
    }
  }

  def asTopOption: Option[SiDimension] = this match {
    case TopDimensionInference => None
    case BottomDimensionInference => {
      throw new RuntimeException("inconsistent dimensions 934587234")
    }
    case ConcreteDimensionInference(dim) => Some(dim)
  }
}

case object TopDimensionInference extends DimensionInference
case object BottomDimensionInference extends DimensionInference
case class ConcreteDimensionInference(dim: SiDimension) extends DimensionInference

object DimensionInference {
  def fromTopOption(mbDim: Option[SiDimension]): DimensionInference = mbDim match {
    case None => TopDimensionInference
    case Some(x) => ConcreteDimensionInference(x)
  }
}
