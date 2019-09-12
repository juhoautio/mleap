package ml.combust.mleap.core.feature

import ml.combust.mleap.core.Model
import ml.combust.mleap.core.types.{ScalarType, StructType}

import language.experimental.macros
import ca.mrvisser.sealerate


sealed trait StringMapHandleInvalid extends EnumStringParam {
  def fromString(value: String): StringMapHandleInvalid = find(value, sealerate.values[StringMapHandleInvalid])
}

object StringMapHandleInvalid {
  val default = Error
  val defaultValue = 0.0

  case object Error extends StringMapHandleInvalid
  case object Keep extends StringMapHandleInvalid
}

/** Class for string map model.
 *
 * Maps a string into a double.
 *
 * @param labels map of labels and values
 * @param handleInvalid how to handle missing labels: 'error' (throw an error),
 *                      or 'keep' (map to the default value)
 * @param defaultValue value to use if label is not found in the map
 */
case class StringMapModel(labels: Map[String, Double],
                          handleInvalid: StringMapHandleInvalid = StringMapHandleInvalid.default,
                          defaultValue: Double = StringMapHandleInvalid.defaultValue) extends Model {

  private val keepInvalid = handleInvalid == StringMapHandleInvalid.Keep

  def apply(label: String): Double = {
    if (keepInvalid) {
      labels.getOrElse(label, defaultValue)
    } else {
      if (labels.contains(label)) {
        labels(label)
      } else {
        throw new NoSuchElementException(s"Missing label: $label. To handle unseen labels, " +
          s"set handleInvalid to ${StringMapHandleInvalid.Keep.asParamString} and optionally set a custom defaultValue")
      }
    }
  }

  override def inputSchema: StructType = StructType("input" -> ScalarType.String).get

  override def outputSchema: StructType = StructType("output" -> ScalarType.Double.nonNullable).get
}
