package ml.combust.mleap.core.feature

/**
 * Lower-cases the name of any extending sealed trait case class.
 *
 * find() can be used to map back from a lower-cased string.
 */
trait EnumStringParam extends Product {

  def asParamString: String = {
    this.productPrefix.toLowerCase
  }

  def find[T <: EnumStringParam](value: String, values: Set[T]) = {
    values.find(_.productPrefix.toLowerCase == value)
      .getOrElse(throw new IllegalArgumentException(s"Invalid handler: $value"))
  }

  def fromString(value: String): EnumStringParam

}
