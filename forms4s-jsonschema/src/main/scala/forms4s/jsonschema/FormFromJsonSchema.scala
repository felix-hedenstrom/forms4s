package forms4s.jsonschema

import cats.data.Ior
import cats.syntax.all.*
import forms4s.FormElement
import forms4s.FormElement.Text.Format
import forms4s.validation.{FormatValidator, RegexValidator, Validator}
import sttp.apispec.{AnySchema, Discriminator, ExampleMultipleValue, ExampleSingleValue, Schema as ASchema, SchemaLike, SchemaType}

import java.util.UUID
import scala.annotation.tailrec
import scala.util.Try
import scala.util.matching.Regex

object FormFromJsonSchema {

  def convert(root: ASchema): FormElement = {
    val result = createElement(None, root, required = true, root.$defs.getOrElse(Map()), None, Nil)
    // TODO we need much better error reporting here, this is wrong on many levels
    //  we probably should just pass errors to the client
    result match {
      case Ior.Right(form)        => form
      case Ior.Left(errors)       =>
        throw new RuntimeException(s"""Failed to generate form from JSON schema.
                                      |${errors.map(_.message).mkString(", ")}""".stripMargin)
      case Ior.Both(errors, form) =>
        // we don't have logging dependency right now, and printlns is the only thing thats available ootb on all platforms
        // This should be replaced with something better soon
        println(s"""Errors spotted when generating form from JSON schema.
                   |${errors.map(_.message).mkString(", ")}""".stripMargin)
        form
    }
  }

  private val MaxRecursionDepth = 3

  private enum ConversionError {
    case Invalid(reason: String)
    case RecursionLimitReached(ref: String)

    def message: String = this match {
      case Invalid(reason)            => reason
      case RecursionLimitReached(ref) => s"Schema $ref is nested more than $MaxRecursionDepth times"
    }
  }

  private type Result = Ior[List[ConversionError], List[FormElement]]

  /** Recursively extract all properties as FormElements */
  private def extractElements(
      schema: ASchema,
      requiredFields: Set[String],
      defs: Map[String, SchemaLike],
      refPath: List[String],
  ): Result = {
    val (recursive, buildable) = schema.properties.toList
      .map({ case (name, subschema) =>
        val isReq = requiredFields.contains(name)
        isReq -> createElement(name.some, subschema, isReq, defs, None, refPath)
      })
      .partition((_, element) => isRecursionLimited(element))
    val recursionErrors        = recursive.flatMap((_, element) => element.left.toList.flatten)
    if (recursive.exists((isReq, _) => isReq)) Ior.left(recursionErrors)
    else
      buildable
        .map((_, element) => element.map(List(_)))
        .combineAllOption
        .getOrElse(Ior.left(if (recursionErrors.isEmpty) List(ConversionError.Invalid("No properties found")) else recursionErrors))
  }

  private def isRecursionLimited(element: Ior[List[ConversionError], FormElement]): Boolean =
    element match {
      case Ior.Left(errors) =>
        errors.forall {
          case ConversionError.RecursionLimitReached(_) => true
          case ConversionError.Invalid(_)               => false
        }
      case _                => false
    }

  private def capitalizeAndSplitWords(str: String): String =
    str.split("(?=\\p{Upper})").map(_.capitalize).mkString(" ")

  @tailrec
  private def createElement(
      nameOverride: Option[String],
      schema: SchemaLike,
      required: Boolean,
      defs: Map[String, SchemaLike],
      parentDiscriminator: Option[String],
      refPath: List[String],
  ): Ior[List[ConversionError], FormElement] = {
    schema match {
      case schema: AnySchema   =>
        val name = nameOverride.getOrElse("")
        schema match {
          case AnySchema.Anything =>
            FormElement.Text(FormElement.Core(name, capitalizeAndSplitWords(name), None, Seq(), required), Format.Raw).rightIor
          case AnySchema.Nothing  => List(ConversionError.Invalid("Nothing schema for a property is not expected")).leftIor
        }
      case unresolved: ASchema =>
        unresolved.$ref match {
          case Some(value) if refPath.count(_ == value) >= MaxRecursionDepth =>
            List(ConversionError.RecursionLimitReached(value)).leftIor
          case Some(value)                                                   =>
            val key = value.split("/").last
            defs.get(key) match {
              case Some(schema) => createElement(nameOverride, schema, required, defs, parentDiscriminator, value :: refPath)
              case None         => List(ConversionError.Invalid(s"Schema for $key not found in $defs")).leftIor
            }
          case None                                                          => handleSchema(nameOverride, unresolved, required, defs, parentDiscriminator, refPath)
        }
    }
  }

  private def handleSchema(
      nameOverride: Option[String],
      schema: ASchema,
      required: Boolean,
      defs: Map[String, SchemaLike],
      parentDiscriminator: Option[String],
      refPath: List[String],
  ): Ior[List[ConversionError], FormElement] = {
    val name                                   = nameOverride.getOrElse(schema.title.getOrElse("unknown"))
    val label                                  = schema.title.getOrElse(capitalizeAndSplitWords(name))
    def core[T](validators: Seq[Validator[T]]) = FormElement.Core(name, label, schema.description, validators, required)

    val variants = schema.oneOf ++ schema.anyOf
    if (variants.nonEmpty) {
      if (variants.size == 2 && variants.exists(isNull)) {
        createElement(nameOverride, variants.find(x => !isNull(x)).get, required = false, defs, None, refPath)
      } else {
        val discriminator = schema.discriminator.map(_.propertyName)
        val built         = variants.map { subSchema =>
          val variantId = schema.discriminator.flatMap(discriminatorValue(_, subSchema, defs))
          createElement(variantId, subSchema, required = true, defs, discriminator, refPath)
        } // TODO required seems fishy
        val kept          = built.filterNot(isRecursionLimited)
        (if (kept.isEmpty) built else kept).sequence
          .map { subElems => FormElement.Alternative(core(Seq()), subElems, discriminator) }
      }
    } else {
      // TODO we dont support alternative representations (multiple schema types)
      //  add error at least
      val tpe = schema.`type`.flatMap(_.headOption)

      tpe match {
        case Some(tpe) =>
          tpe match {
            case SchemaType.Boolean => FormElement.Checkbox(core(Seq())).rightIor
            case SchemaType.Object  =>
              extractElements(schema, schema.required.toSet, defs, refPath)
                .map(subElems => FormElement.Group(core(Seq()), subElems.filter(x => !parentDiscriminator.contains(x.core.id))))
            case SchemaType.Array   =>
              schema.items
                .map(schema => createElement(None, schema, required = true, defs, None, refPath))
                .getOrElse(List(ConversionError.Invalid(s"No items schema for array $name")).leftIor)
                .map(itemElem => FormElement.Multivalue(core(Seq()), itemElem))
            case SchemaType.Number  => FormElement.Number(core(Seq()), isInteger = false).rightIor
            case SchemaType.Integer => FormElement.Number(core(Seq()), isInteger = true).rightIor
            case SchemaType.String  =>
              for {
                enumOptions <- schema.`enum`.getOrElse(Nil).traverse {
                                 case ExampleSingleValue(value) => value.toString.rightIor
                                 case ExampleMultipleValue(_)   => List(ConversionError.Invalid("Multivalue options not supported for enums")).leftIor
                               }
              } yield {
                if (enumOptions.nonEmpty) {
                  FormElement.Select(core(Seq()), enumOptions)
                } else {
                  val format = schema.format.map(_.toLowerCase)
                  val maxLen = schema.maxLength.getOrElse(100)
                  val minLen = schema.minLength.getOrElse(100)

                  // TODO validators
                  val (validators, tFormat) = format match {
                    case Some("date")         => Seq() -> Format.Date
                    case Some("multiline")    => Seq() -> Format.Multiline
                    case Some("time")         => Seq() -> Format.Time
                    case Some("date-time")    => Seq() -> Format.DateTime
                    case Some("email")        =>
                      val emailRegex = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$".r
                      Seq(FormatValidator("email", x => emailRegex.matches(x), Some("user@example.com"))) -> Format.Email
                    case Some(x @ "uuid")     =>
                      Seq(FormatValidator("UUIDv4", x => Try(UUID.fromString(x)).isSuccess, Some("d3399597-a3b6-4813-ac0a-23bb84a95e11"))) -> Format
                        .Custom(x)
                    case Some(x @ "duration") => Seq() -> Format.Custom(x)
                    case Some(x)              => Seq() -> Format.Custom(x)
                    case None                 =>
                      val isMultiline = maxLen > 120 || minLen > 120
                      val format      = if (isMultiline) Format.Multiline else Format.Raw
                      Seq() -> format
                  }
                  val patternOpt            = schema.pattern.map(p => RegexValidator(Regex(p.value)))
                  FormElement.Text(core(validators ++ patternOpt.toList), tFormat)
                }
              }

            case SchemaType.Null => List(ConversionError.Invalid("Null schema for a property is not expected")).leftIor
          }
        case None      => List(ConversionError.Invalid("Schema type not specified")).leftIor
      }
    }
  }

  private def discriminatorValue(discriminator: Discriminator, variant: SchemaLike, defs: Map[String, SchemaLike]): Option[String] = {
    val fromMapping = refOf(variant).flatMap(ref => discriminator.mapping.flatMap(_.collectFirst { case (value, `ref`) => value }))
    fromMapping.orElse(resolve(variant, defs).flatMap(constantOf(_, discriminator.propertyName)))
  }

  private def constantOf(schema: ASchema, property: String): Option[String] =
    schema.properties
      .get(property)
      .flatMap {
        case propertySchema: ASchema => propertySchema.`const`.orElse(propertySchema.`enum`.collect { case List(single) => single })
        case _: AnySchema            => None
      }
      .collect { case ExampleSingleValue(value: String) => value }

  private def refOf(schema: SchemaLike): Option[String] =
    schema match {
      case schema: ASchema => schema.$ref
      case _: AnySchema    => None
    }

  @tailrec
  private def resolve(schema: SchemaLike, defs: Map[String, SchemaLike]): Option[ASchema] =
    schema match {
      case _: AnySchema    => None
      case schema: ASchema =>
        schema.$ref match {
          case Some(ref) =>
            defs.get(ref.split("/").last) match {
              case Some(resolved) => resolve(resolved, defs)
              case None           => None
            }
          case None      => Some(schema)
        }
    }

  def isNull(schema: SchemaLike): Boolean = {
    schema match {
      case _: AnySchema    => false
      case schema: ASchema => schema.`type`.contains(List(SchemaType.Null))
    }

  }

}
