package forms4s.circe

import forms4s.FormElement.Text.Format
import forms4s.{FormElement, FormElementState, FormElementUpdate}
import io.circe.Json
import io.circe.syntax.KeyOps
import org.scalatest.freespec.AnyFreeSpec

class FormStateToJsonTest extends AnyFreeSpec {

  "optional fields" - {
    val optionalText   = FormElement.Text(FormElement.Core("note", "Note", None, Seq(), required = false), Format.Raw)
    val optionalNumber = FormElement.Number(FormElement.Core("count", "Count", None, Seq(), required = false), isInteger = true)
    val requiredText   = FormElement.Text(FormElement.Core("name", "Name", None, Seq()), Format.Raw)
    val group          = FormElement.Group(FormElement.Core("root", "Root", None, Seq()), List(optionalText, optionalNumber, requiredText))

    "are omitted when left blank" in {
      val state = FormElementState.empty(group)
      assert(state.extractJson == Json.obj("name" := ""))
    }

    "are included when filled" in {
      val state = FormElementState
        .empty(group)
        .update(FormElementUpdate.Nested(0, FormElementUpdate.ValueUpdate("hello")))
        .update(FormElementUpdate.Nested(1, FormElementUpdate.ValueUpdate(Some(3.0))))
      assert(state.extractJson == Json.obj("note" := "hello", "count" := 3.0, "name" := ""))
    }
  }

  "alternative writes the variant id as discriminator value" in {
    val variantA    = FormElement.Group(FormElement.Core("A_VARIANT", "AVariant", None, Seq()), List())
    val variantB    = FormElement.Group(FormElement.Core("B_VARIANT", "BVariant", None, Seq()), List())
    val alternative = FormElement.Alternative(FormElement.Core("alt", "Alt", None, Seq()), Seq(variantA, variantB), Some("tpe"))

    val state = FormElementState.empty(alternative).update(FormElementUpdate.AlternativeSelected(1))
    assert(state.extractJson == Json.obj("tpe" := "B_VARIANT"))
  }

}
