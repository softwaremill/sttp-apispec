package sttp.apispec.validation

case class Bound[T](value: T, inclusive: Boolean)
object Bound {
  def inclusive[T](value: T): Bound[T] = Bound(value, inclusive = true)
  def exclusive[T](value: T): Bound[T] = Bound(value, inclusive = false)
}

case class Bounds[T](min: Option[Bound[T]], max: Option[Bound[T]]) {
  override def toString: String = {
    val minRepr = min.fold("(-inf") {
      case Bound(value, false) => s"($value"
      case Bound(value, true)  => s"[$value"
    }
    val maxRepr = max.fold("inf)") {
      case Bound(value, false) => s"$value)"
      case Bound(value, true)  => s"$value]"
    }
    s"$minRepr,$maxRepr"
  }

  def contains(other: Bounds[T])(implicit ord: Ordering[T]): Boolean = {
    val minOk = (min, other.min) match {
      case (Some(Bound(tm, false)), Some(Bound(om, true))) => ord.lt(tm, om)
      case (Some(Bound(tm, _)), Some(Bound(om, _)))        => ord.lteq(tm, om)
      case (Some(_), None)                                 => false
      case (None, _)                                       => true
    }
    val maxOk = (max, other.max) match {
      case (Some(Bound(tm, false)), Some(Bound(om, true))) => ord.gt(tm, om)
      case (Some(Bound(tm, _)), Some(Bound(om, _)))        => ord.gteq(tm, om)
      case (Some(_), None)                                 => false
      case (None, _)                                       => true
    }
    minOk && maxOk
  }
}
