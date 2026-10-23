package forms4s.circe

import forms4s._
import io.circe._
import io.circe.syntax._

object FormStateToJson {

  def extract(element: FormElementState): Json = element match {
    case x: FormElementState.Text        => Json.fromString(x.value)
    case x: FormElementState.Number      =>
      Json.fromDoubleOrNull(x.value)
    case x: FormElementState.Checkbox    => Json.fromBoolean(x.value)
    case x: FormElementState.Select      => Json.fromString(x.value)
    case x: FormElementState.Group       => Json.fromFields(x.value.filterNot(isBlankOptional).map(elem => elem.id -> extract(elem)))
    case x: FormElementState.Multivalue  => Json.fromValues(x.value.map(extract))
    case x: FormElementState.Alternative =>
      val base              = extract(x.value.states(x.value.selected))
      val withDiscriminator = for {
        asObj <- base.asObject
        disc  <- x.element.discriminator
      } yield asObj.+:(disc -> Json.fromString(x.element.variants(x.value.selected).core.id)).asJson
      withDiscriminator.getOrElse(base)
  }

  private def isBlankOptional(element: FormElementState): Boolean =
    !element.element.core.required && (element match {
      case x: FormElementState.Text   => x.value.isEmpty
      case x: FormElementState.Number => x.value.isEmpty
      case _                          => false
    })

  // Extension method for easier conversion
  extension (formState: FormElementState) def extractJson: Json = extract(formState)
}
