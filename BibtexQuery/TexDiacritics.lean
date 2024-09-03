/-
Copyright (c) 2024 Jz Pan. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Jz Pan
-/

import Std.Internal.Parsec
import Std.Internal.Parsec.String

/-!

# TeX diacritics

This file contains functions for TeX diacritics processing.
The main function is `texDiacritics`, which
will convert all TeX commands for diacritics into UTF-8 characters,
and error on any other TeX commands which are not in math environment.

-/

open Lean Std.Internal.Parsec Std.Internal.Parsec.String

namespace BibtexQuery.TexDiacritics

/-- Match a sequence of space characters and return it. -/
def ws' : Parser String :=  manyChars <| satisfy fun c =>
  c == ' ' || c == '\n' || c == '\r' || c == '\t'

/-- Match a normal character which is not the beginning of TeX command. -/
def normalChar : Parser Char := satisfy fun c =>
  c != '\\' && c != '$' && c != '{' && c != '}'

/-- Replace certain sequences (e.g. "--") by their UTF-8 representations. -/
def replaceChars (s : String) : String :=
  let arr : Array (String × String) := #[
    ("---", "\u2014"),
    ("--", "\u2013"),
    ("~", "\u00A0"),
    ("?`", "\u00BF"),
    ("!`", "\u00A1")
  ]
  arr.foldl (fun acc (o, r) => acc.replace o r) s

/-- Match at least one normal characters which is not the beginning of TeX command. -/
def normalChars : Parser String := do
  let s ← many1Chars normalChar
  pure <| replaceChars s

/-- Match a TeX command starting with `\`, potentially with trailing whitespaces. -/
def texCommand : Parser String := pchar '\\' *> attempt do
  let s ← manyChars asciiLetter
  if s.isEmpty then
    return "\\" ++ toString (← any) ++ (← ws')
  else
    match ← peek? with
    | .some c =>
      match c with
      | '*' =>
        skip
        return "\\" ++ s ++ toString c ++ (← ws')
      | _ =>
        return "\\" ++ s ++ (← ws')
    | .none =>
      return "\\" ++ s

/-- Similar to `texCommand` but it excludes some commands. -/
def texCommand' (exclude : Array String) : Parser String := attempt do
  let s ← texCommand
  match exclude.find? (· == s.trim) with
  | .some _ => fail s!"'{s.trim}' is not allowed"
  | .none => return s

/-- Match a sequence starting with `{` and ending with `}`. -/
def bracedContent (p : Parser String) : Parser String :=
  pchar '{' *> (("{" ++ · ++ "}") <$> p) <* pchar '}'

/-- Similar to `bracedContent` but it does not output braces. -/
def bracedContent' (p : Parser String) : Parser String :=
  pchar '{' *> p <* pchar '}'

partial def manyOptions {α} (p : Parser (Option α)) (acc : Array α := #[]) :
    Parser (Array α) := fun it =>
  match p it with
  | .success it ret =>
    match ret with
    | .some ret => manyOptions p (acc.push ret) it
    | .none => .success it acc
  | .error it err => .error it err

partial def mathContentAux : Parser String := do
  let doOne : Parser (Option String) := fun it =>
    if it.hasNext then
      match it.curr with
      | '{' => (.some <$> bracedContent mathContentAux) it
      | '\\' =>
        match texCommand' #["\\(", "\\)", "\\[", "\\]"] it with
        | .success it ret => .success it (.some ret)
        | .error _ _ => .success it .none
      | '}' | '$' => .success it .none
      | _ => (.some <$> normalChars) it
    else
      .success it .none
  return String.join (← manyOptions doOne).toList

/-- Match a math content. Returns `Option.none` if it does not start with `\(`, `\[` or `$`. -/
def mathContent : Parser (Option String) := fun it =>
  let aux (beginning ending dollar : String) : Parser String :=
    pstring beginning *> ((dollar ++ · ++ dollar) <$> mathContentAux) <* pstring ending
  let substr := it.extract (it.forward 2)
  if substr = "\\[" then
    (.some <$> aux "\\[" "\\]" "$$") it
  else if substr = "\\(" then
    (.some <$> aux "\\(" "\\)" "$") it
  else if substr = "$$" then
    (.some <$> aux "$$" "$$" "$$") it
  else if it.curr = '$' then
    (.some <$> aux "$" "$" "$") it
  else
    .success it .none

/-- Match a TeX command for diacritics, return the corresponding UTF-8 string.
Sometimes it needs to read the character after the command,
in this case the `p` is used to read braced content. -/
def texDiacriticsCommand (p : Parser String) : Parser String := do
  let cmd ← String.trim <$> texCommand
  match cmd with
  | "\\oe" => pure "œ" | "\\OE" => pure "Œ"
  | "\\ae" => pure "æ" | "\\AE" => pure "Æ"
  | "\\aa" => pure "å" | "\\AA" => pure "Å"
  | "\\o" => pure "ø" | "\\O" => pure "Ø"
  | "\\l" => pure "ł" | "\\L" => pure "Ł"
  | "\\i" => pure "ı" | "\\j" => pure "ȷ"
  | "\\ss" => pure "\u00DF" | "\\SS" => pure "\u1E9E"
  | "\\cprime" => pure "\u02B9"
  | "\\&" => pure "&"
  | "\\" => pure "\u00A0" -- This should be "\ " but the space is trimmed
  | _ =>
    let ch : String := match cmd with
    | "\\`" => "\u0300" | "\\'" => "\u0301"
    | "\\^" => "\u0302" | "\\\"" => "\u0308"
    | "\\~" => "\u0303" | "\\=" => "\u0304"
    | "\\." => "\u0307" | "\\u" => "\u0306"
    | "\\v" => "\u030C" | "\\H" => "\u030B"
    | "\\t" => "\u0361" | "\\c" => "\u0327"
    | "\\d" => "\u0323" | "\\b" => "\u0331"
    | "\\k" => "\u0328"
    | _ => ""
    if ch.isEmpty then
      fail s!"unsupported command: '{cmd}'"
    else
      let doOne : Parser String := fun it =>
        if it.hasNext then
          match it.curr with
          | '{' => bracedContent p it
          | _ => normalChars it
        else
          .error it "character expected"
      let s ← doOne
      if s.startsWith "{" then
        if s.length < 3 then
          fail s!"expected string of length at least 3, but got '{s}'"
        else
          return s.take 2 ++ ch ++ s.drop 2
      else
        if s.isEmpty then
          fail "expected a non-empty string"
        else
          return s.take 1 ++ ch ++ s.drop 1

/-- Convert all TeX commands for diacritics into UTF-8 characters,
and error on any other TeX commands which are not in math environment. -/
partial def texDiacritics : Parser String := do
  let doOne : Parser (Option String) := fun it =>
    if it.hasNext then
      match mathContent it with
      | .success it ret =>
        match ret with
        | .some ret => .success it (.some ret)
        | .none =>
          match it.curr with
          | '{' => (.some <$> bracedContent texDiacritics) it
          | '\\' => (.some <$> texDiacriticsCommand texDiacritics) it
          | '}' => .success it .none
          | _ => (.some <$> normalChars) it
      | .error it err => .error it err
    else
      .success it .none
  return String.join (← manyOptions doOne).toList

/-- Remove all braces except for those in math environment,
and error on any TeX commands which are not in math environment. -/
partial def removeBraces : Parser String := do
  let doOne : Parser (Option String) := fun it =>
    if it.hasNext then
      match mathContent it with
      | .success it ret =>
        match ret with
        | .some ret => .success it (.some ret)
        | .none =>
          match it.curr with
          | '{' => (.some <$> bracedContent' removeBraces) it
          | '}' => .success it .none
          | _ => (.some <$> normalChars) it
      | .error it err => .error it err
    else
      .success it .none
  return String.join (← manyOptions doOne).toList

end BibtexQuery.TexDiacritics
