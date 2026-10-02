/-
Copyright (c) 2022 Frédéric Dupuis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Frédéric Dupuis
-/

import BibtexQuery.ParsecExtra
import BibtexQuery.Entry
import Std.Internal.Parsec
import Std.Internal.Parsec.String

/-!
# Bibtex Parser

This file contains a parser for the Bibtex format. Note that currently, only a subset of the official
Bibtex format is supported; features such as `@string` macros and `crossref` inheritance are not
supported.
-/

open Lean Std.Internal.Parsec Std.Internal.Parsec.String BibtexQuery.ParsecExtra

namespace BibtexQuery.Parser

def isIdentifierChar (c : Char) : Bool :=
  !c.isWhitespace && !"\"#%'(),={}".contains c

/-- A BibTeX identifier.
  BibTeX compares them case-insensitively, so the result is lowercased.
  This matches bibtex's `scan_identifier` rule.
-/
def identifier : Parser String := attempt do
  let first ← satisfy fun c => isIdentifierChar c && !c.isDigit
  let rest ← manyChars (satisfy isIdentifierChar)
  return (first.toString ++ rest).toLower

/-- The cite key of an entry (i.e. what goes in the cite command), delimited on the right by
`rightDelim` (`}` or `)`). BibTeX reads it up to the next whitespace, comma or, in a brace-delimited
entry, closing brace (`scan2_white`/`scan1_white`); it may contain any other character, such as
`.`, `/` or non-ASCII letters, and its case is kept. Unlike BibTeX, an empty key is rejected. -/
def name (rightDelim : Char := '}') : Parser String := do
  let s ← manyChars <| satisfy fun c =>
    !c.isWhitespace && c != ',' && (rightDelim != '}' || c != '}')
  if s.isEmpty then fail "cite key expected" else return s

/-- "article", "book", etc -/
def category : Parser String := attempt do skipChar '@'; ws; identifier

partial def bracedContentTail (acc : String) : Parser String := attempt do
  let c ← any
  if c = '{' then
    let s ← bracedContentTail ""
    bracedContentTail (acc ++ "{" ++ s)
  else
    if c = '}' then return acc ++ "}"
    else
      bracedContentTail (acc ++ c.toString)

/-- A brace-delimited string; braces inside must balance. -/
def bracedContent : Parser String := attempt do
  skipChar '{'
  let s ← bracedContentTail ""
  return s.dropEnd 1 |>.copy

partial def quotedContentTail (depth : Nat) (acc : String) : Parser String := do
  let c ← any
  match c with
  | '"' => if depth = 0 then return acc else quotedContentTail depth (acc.push c)
  | '{' => quotedContentTail (depth + 1) (acc.push c)
  | '}' =>
    if depth = 0 then fail "unbalanced braces in quoted string"
    else quotedContentTail (depth - 1) (acc.push c)
  | _ => quotedContentTail depth (acc.push c)

/-- A quote-delimited string. As in BibTeX, a `"` inside braces does not end the string, and a
backslash does not escape anything. Line breaks are replaced by spaces. -/
def quotedContent : Parser String := attempt do
  skipChar '"'
  let s ← quotedContentTail 0 ""
  return ((s.replace "\r\n" " ").replace "\n" " ").replace "\r" " "

/-- We do not support string macros in general, but we do hard code the months, one of the most common cases. -/
def stringMacro : Parser String := attempt do
  let s ← identifier
  if ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"].contains s
  then return s
  else fail s!"Not a supported string macro: '{s}'"

/-- One piece of a field value: a braced string, a quoted string, a number or a month macro. -/
def fieldPiece : Parser String := do
  match ← peek? with
  | some '"' => quotedContent
  | some '{' => bracedContent
  | some c =>
    if c.isDigit then manyChars digit
    else if isIdentifierChar c then stringMacro
    else fail "field value expected"
  | none => fail "field value expected"

/-- The content field of a tag: one or more pieces concatenated with `#`. -/
def tagContent : Parser String := do
  let first ← fieldPiece
  let rest ← many' (attempt do ws; skipChar '#'; ws; fieldPiece)
  return rest.foldl (· ++ ·) first

/-- i.e. journal = {Journal of Musical Deontology} -/
def tag : Parser Tag := do
  let tagName ← identifier
  ws; skipChar '='; ws
  let tagContent ← tagContent
  return { name := tagName, content := tagContent }

/-- Text outside of entries, up to the next `@` or the end of the input. -/
def outsideEntry : Parser Unit := do
  let _ ← manyChars <| noneOf "@"

/-- The fields of an entry, after its cite key, up to and including the closing delimiter. The
fields may be absent, and a trailing comma is allowed. -/
def entryFields (rightDelim : Char) : Parser (List Tag) := do
  ws
  if (← peek?) == some ',' then
    skip; ws
    let t ← sepOrEndBy tag (do ws; skipChar ','; ws)
    ws; skipChar rightDelim
    return t
  else
    skipChar rightDelim
    return []

/-- The body of an entry of the given (lowercased) type, after `@type`. -/
def entryBody (typeOfEntry : String) : Parser Entry := do
  -- BibTeX ignores the rest of `@comment`: no body is read, and the text after it is text
  -- outside of entries.
  if typeOfEntry = "comment" then return .commentType
  ws
  let leftDelim ← satisfy (fun c => c = '{' ∨ c = '(') <|> fail "'{' or '(' expected"
  let rightDelim := if leftDelim = '{' then '}' else ')'
  ws
  match typeOfEntry with
  | "preamble" =>
    let s ← tagContent
    ws; skipChar rightDelim
    return .preambleType s
  | "string" =>
    let t ← tag
    ws; skipChar rightDelim
    return .stringType t.toString
  | _ =>
    let nom ← name rightDelim
    let t ← entryFields rightDelim
    return Entry.normalType typeOfEntry nom t

/-- A Bibtex entry (including the commands `@comment`, `@preamble` and `@string`), preceded by
any text outside of entries. Once the `@` is read, a malformed entry is an error. -/
def entry : Parser Entry := do
  outsideEntry
  let typeOfEntry ← category <|> fail "entry type expected after '@'"
  entryBody typeOfEntry

partial def bibtexFileCore (acc : Array Entry) : Parser (List Entry) := do
  outsideEntry
  if ← isEof then return acc.toList
  let e ← entry
  bibtexFileCore (acc.push e)

def bibtexFile : Parser (List Entry) := bibtexFileCore #[]

end BibtexQuery.Parser
