/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.tc

import com.whatsapp.eqwalizer.ast.Exprs.Expr
import com.whatsapp.eqwalizer.ast.{Id, Pos}
import com.whatsapp.eqwalizer.util.Diagnostic.Diagnostic

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

case class RawEqwalizerStrictAttributes(
    disabledWarnings: Set[(String, Id)] = Set.empty,
    privateConstructorOwners: Map[String, Set[String]] = Map.empty,
    invalids: List[Diagnostic] = Nil,
)

object RawEqwalizerStrictAttributes {
  private val StrictAttributeName = "-eqwalizer_strict"
  private val AtomPattern = "[a-z][a-zA-Z0-9_@]*"
  private val SupportedFlags = Set("disable_warning", "private_constructor")

  private val DisableWarning =
    s"""^\\s*-eqwalizer_strict\\(\\{disable_warning,\\s*($AtomPattern),\\s*($AtomPattern)/([0-9]+)\\}\\)\\.\\s*(?:%.*)?$$""".r
  private val PrivateConstructor =
    s"""^\\s*-eqwalizer_strict\\(\\{private_constructor,\\s*($AtomPattern),\\s*($AtomPattern)\\}\\)\\.\\s*(?:%.*)?$$""".r
  private val FlagLine =
    s"""^\\s*-eqwalizer_strict\\(\\{\\s*($AtomPattern).*""".r

  case class InvalidEqwalizerStrictAttribute(pos: Pos, line: String, reason: String) extends Diagnostic {
    override val msg: String = s"Invalid eqwalizer_strict attribute: $reason\nLine: $line"
    override val errorName: String = "invalid_eqwalizer_strict_attribute"
    override val erroneousExpr: Option[Expr] = None
  }

  def load(path: String): RawEqwalizerStrictAttributes =
    try {
      val bytes = Files.readAllBytes(Paths.get(path))
      parse(new String(bytes, StandardCharsets.UTF_8))
    } catch {
      case NonFatal(_) => RawEqwalizerStrictAttributes()
    }

  private def parse(text: String): RawEqwalizerStrictAttributes = {
    var offset = 0
    var disabledWarnings = Set.empty[(String, Id)]
    var privateConstructorOwners = Map.empty[String, Set[String]]
    val invalids = ListBuffer.empty[Diagnostic]

    for (chunk <- text.split("(?<=\\n)", -1).toList if chunk.nonEmpty) {
      val line = chunk.stripSuffix("\n").stripSuffix("\r")
      val start = offset
      val end = start + line.getBytes(StandardCharsets.UTF_8).length
      offset += chunk.getBytes(StandardCharsets.UTF_8).length

      if (shouldInspect(line)) {
        val pos = Pos.TextRange(start, end)
        line match {
          case DisableWarning(warningName, funName, arity) =>
            disabledWarnings += ((warningName, Id(funName, arity.toInt)))
          case PrivateConstructor(recordName, ownerModule) =>
            val owners = privateConstructorOwners.getOrElse(recordName, Set.empty)
            privateConstructorOwners += recordName -> (owners + ownerModule)
          case FlagLine(flag) if SupportedFlags.contains(flag) =>
            invalids.addOne(InvalidEqwalizerStrictAttribute(pos, line, expectedSyntax(flag)))
          case FlagLine(flag) =>
            invalids.addOne(
              InvalidEqwalizerStrictAttribute(
                pos,
                line,
                s"unsupported flag `$flag`; supported flags are: ${SupportedFlags.toList.sorted.mkString(", ")}",
              )
            )
          case _ =>
            invalids.addOne(
              InvalidEqwalizerStrictAttribute(
                pos,
                line,
                s"expected a one-line $StrictAttributeName attribute with a supported flag",
              )
            )
        }
      }
    }

    RawEqwalizerStrictAttributes(disabledWarnings, privateConstructorOwners, invalids.toList)
  }

  private def shouldInspect(line: String): Boolean =
    !line.trim.startsWith("%") && line.contains(StrictAttributeName)

  private def expectedSyntax(flag: String): String =
    flag match {
      case "disable_warning" =>
        "expected -eqwalizer_strict({disable_warning, WarningName, Fun/Arity})."
      case "private_constructor" =>
        "expected -eqwalizer_strict({private_constructor, RecordName, OwnerModule})."
      case _ =>
        s"unsupported flag `$flag`; supported flags are: ${SupportedFlags.toList.sorted.mkString(", ")}"
    }
}
