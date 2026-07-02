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
    disabledErrors: Set[(String, Id)] = Set.empty,
    privateConstructorOwners: Map[String, Set[String]] = Map.empty,
    invalids: List[Diagnostic] = Nil,
)

object RawEqwalizerStrictAttributes {
  private val AttributeName = "eqwalizer_strict"
  private val AtomPattern = "[a-z][a-zA-Z0-9_@]*"
  private val SupportedFlags = Set("disable_error", "private_constructor")
  private val SupportedErrors = Set("skipped_exhaustiveness_check")

  private case class SourceLine(text: String, startByte: Int, endByte: Int) {
    val pos: Pos = Pos.TextRange(startByte, endByte)
  }

  private val Directive =
    s"""^\\s*%+\\s*eqwalizer_strict:($AtomPattern)(?:\\s+(.*?))?\\s*$$""".r
  private val CommentMention =
    s"""^\\s*%+.*\\beqwalizer_strict\\b.*$$""".r
  private val NonCommentMention =
    s"""^\\s*[^%].*\\beqwalizer_strict\\b.*$$""".r
  private val SpecStart =
    s"""^\\s*-spec\\s+.*""".r
  private val SpecId =
    s"""^\\s*-spec\\s+($AtomPattern)\\s*\\((.*)\\)\\s*->.*""".r
  private val Record =
    s"""^\\s*-record\\s*\\(\\s*($AtomPattern)\\s*,.*""".r

  case class InvalidEqwalizerStrictAttribute(pos: Pos, line: String, reason: String) extends Diagnostic {
    override val msg: String = s"Invalid eqwalizer_strict directive: $reason\nLine: $line"
    override val errorName: String = "invalid_eqwalizer_strict_attribute"
    override val erroneousExpr: Option[Expr] = None
  }

  case class UnreadableEqwalizerStrictFile(path: String, reason: String) extends Diagnostic {
    override val pos: Pos = Pos.TextRange(0, 0)
    override val msg: String = s"Could not read source file for eqwalizer_strict directives: $path\nReason: $reason"
    override val errorName: String = "unreadable_eqwalizer_strict_file"
    override val erroneousExpr: Option[Expr] = None
  }

  def load(path: String): RawEqwalizerStrictAttributes =
    try {
      val bytes = Files.readAllBytes(Paths.get(path))
      parse(new String(bytes, StandardCharsets.UTF_8))
    } catch {
      case NonFatal(e) =>
        RawEqwalizerStrictAttributes(invalids = List(UnreadableEqwalizerStrictFile(path, e.getMessage)))
    }

  private def parse(text: String): RawEqwalizerStrictAttributes = {
    val lines = sourceLines(text)
    var disabledErrors = Set.empty[(String, Id)]
    var privateConstructorOwners = Map.empty[String, Set[String]]
    val invalids = ListBuffer.empty[Diagnostic]
    var i = 0

    while (i < lines.size) {
      val line = lines(i)
      line.text match {
        case Directive(flag, args) if flag == "disable_error" =>
          val errorName = Option(args).map(_.trim).getOrElse("")
          if (!SupportedErrors.contains(errorName)) {
            invalids.addOne(
              InvalidEqwalizerStrictAttribute(
                line.pos,
                line.text,
                s"unsupported error `$errorName`; supported errors are: ${SupportedErrors.toList.sorted.mkString(", ")}",
              )
            )
            i += 1
          } else {
            parseDisableError(lines, i, errorName) match {
              case Right((id, nextIndex)) =>
                disabledErrors += ((errorName, id))
                i = nextIndex
              case Left(reason) =>
                invalids.addOne(InvalidEqwalizerStrictAttribute(line.pos, line.text, reason))
                i += 1
            }
          }
        case Directive(flag, args) if flag == "private_constructor" =>
          val ownerModule = Option(args).map(_.trim).getOrElse("")
          if (!ownerModule.matches(AtomPattern)) {
            invalids.addOne(
              InvalidEqwalizerStrictAttribute(
                line.pos,
                line.text,
                "expected % eqwalizer_strict:private_constructor OwnerModule, where OwnerModule is an atom",
              )
            )
            i += 1
          } else {
            parsePrivateConstructor(lines, i, ownerModule) match {
              case Right((recordName, nextIndex)) =>
                val owners = privateConstructorOwners.getOrElse(recordName, Set.empty)
                privateConstructorOwners += recordName -> (owners + ownerModule)
                i = nextIndex
              case Left(reason) =>
                invalids.addOne(InvalidEqwalizerStrictAttribute(line.pos, line.text, reason))
                i += 1
            }
          }
        case Directive(flag, _) =>
          val reason =
            if (SupportedFlags.contains(flag)) expectedSyntax(flag)
            else s"unsupported flag `$flag`; supported flags are: ${SupportedFlags.toList.sorted.mkString(", ")}"
          invalids.addOne(InvalidEqwalizerStrictAttribute(line.pos, line.text, reason))
          i += 1
        case CommentMention() =>
          invalids.addOne(
            InvalidEqwalizerStrictAttribute(
              line.pos,
              line.text,
              s"expected % $AttributeName:Flag Args",
            )
          )
          i += 1
        case NonCommentMention() =>
          invalids.addOne(
            InvalidEqwalizerStrictAttribute(
              line.pos,
              line.text,
              s"$AttributeName directives must be comments",
            )
          )
          i += 1
        case _ =>
          i += 1
      }
    }

    RawEqwalizerStrictAttributes(disabledErrors, privateConstructorOwners, invalids.toList)
  }

  private def parseDisableError(lines: List[SourceLine], directiveIndex: Int, errorName: String): Either[String, (Id, Int)] = {
    val specIndex = directiveIndex + 1
    if (!lines.isDefinedAt(specIndex) || !isSpecStart(lines(specIndex).text))
      Left("disable_error must be immediately followed by a -spec")
    else {
      val (specText, nextIndex) = collectSpec(lines, specIndex)
      parseSpecId(specText) match {
        case Some(id) => Right((id, nextIndex))
        case None     => Left(s"could not parse -spec following disable_error for `$errorName`")
      }
    }
  }

  private def parsePrivateConstructor(lines: List[SourceLine], directiveIndex: Int, ownerModule: String): Either[String, (String, Int)] = {
    val recordIndex = directiveIndex + 1
    if (!lines.isDefinedAt(recordIndex))
      Left("private_constructor must be immediately followed by a -record")
    else
      lines(recordIndex).text match {
        case Record(recordName) => Right((recordName, recordIndex + 1))
        case _                  => Left(s"private_constructor for `$ownerModule` must be immediately followed by a -record")
      }
  }

  private def isSpecStart(line: String): Boolean =
    line match {
      case SpecStart() => true
      case _           => false
    }

  private def collectSpec(lines: List[SourceLine], specIndex: Int): (String, Int) = {
    val specLines = ListBuffer(lines(specIndex).text)
    var nextIndex = specIndex + 1
    while (lines.isDefinedAt(nextIndex) && startsWithWhitespace(lines(nextIndex).text)) {
      specLines.addOne(lines(nextIndex).text)
      nextIndex += 1
    }
    (specLines.toList.mkString(" "), nextIndex)
  }

  private def parseSpecId(specText: String): Option[Id] =
    specText match {
      case SpecId(funName, args) => Some(Id(funName, specArity(args)))
      case _                     => None
    }

  private def specArity(args: String): Int = {
    val trimmed = args.trim
    if (trimmed.isEmpty) 0
    else 1 + countTopLevelCommas(trimmed)
  }

  private def countTopLevelCommas(args: String): Int = {
    var parenDepth = 0
    var braceDepth = 0
    var bracketDepth = 0
    var angleDepth = 0
    var commas = 0
    for (ch <- args) {
      ch match {
        case '(' => parenDepth += 1
        case ')' if parenDepth > 0 => parenDepth -= 1
        case '{' => braceDepth += 1
        case '}' if braceDepth > 0 => braceDepth -= 1
        case '[' => bracketDepth += 1
        case ']' if bracketDepth > 0 => bracketDepth -= 1
        case '<' => angleDepth += 1
        case '>' if angleDepth > 0 => angleDepth -= 1
        case ',' if parenDepth == 0 && braceDepth == 0 && bracketDepth == 0 && angleDepth == 0 => commas += 1
        case _ =>
      }
    }
    commas
  }

  private def startsWithWhitespace(line: String): Boolean =
    line.headOption.exists(ch => ch == ' ' || ch == '\t')

  private def sourceLines(text: String): List[SourceLine] = {
    var offset = 0
    val result = ListBuffer.empty[SourceLine]
    for (chunk <- text.split("(?<=\\n)", -1).toList if chunk.nonEmpty) {
      val line = chunk.stripSuffix("\n").stripSuffix("\r")
      val start = offset
      val end = start + line.getBytes(StandardCharsets.UTF_8).length
      offset += chunk.getBytes(StandardCharsets.UTF_8).length
      result.addOne(SourceLine(line, start, end))
    }
    result.toList
  }

  private def expectedSyntax(flag: String): String =
    flag match {
      case "disable_error" =>
        "expected % eqwalizer_strict:disable_error skipped_exhaustiveness_check immediately followed by a -spec"
      case "private_constructor" =>
        "expected % eqwalizer_strict:private_constructor OwnerModule immediately followed by a -record"
      case _ =>
        s"unsupported flag `$flag`; supported flags are: ${SupportedFlags.toList.sorted.mkString(", ")}"
    }
}
