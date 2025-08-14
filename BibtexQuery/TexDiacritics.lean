/-
Copyright (c) 2024 Jz Pan. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Jz Pan
-/

import Std.Internal.Parsec
import Std.Internal.Parsec.String
import Lean.Data.Xml.Basic
import UnicodeBasic

/-!

# TeX diacritics

This file contains functions for TeX diacritics processing.
The main functions are `texContent` and `texContents`, which
will convert all TeX commands for diacritics into UTF-8 characters,
and preserve other TeX commands.

-/

open Lean Xml Std.Internal.Parsec Std.Internal.Parsec.String Unicode

namespace BibtexQuery.TexDiacritics

/-- Represents a segment of TeX content of in bibitem. -/
inductive TexContent
/-- Represents a non-empty normal string. -/
| normal (s : String) : TexContent
/-- Represents some special characters. When output into HTML, some of them (e.g. `\`, `$`)
will be put into `<span>` to prevent MathJax from recognizing them.
Some of them (e.g. ` `, `,`) have special meaning in bibitem name processing. -/
| char (c : Char) : TexContent
/-- Represents a TeX command. It always starts with `\`. It may have trailing spaces. -/
| command (s : String) : TexContent
/-- Represents a math environment. -/
| math (dollar s : String) : TexContent
/-- Represents contents inside `{ ... }`. -/
| braced (arr : Array TexContent) : TexContent
deriving Repr

namespace TexContent

/-- Add a diacritics character after the first character of the content. -/
partial def addDiacritics (x : TexContent) (ch : String) :
    Except String TexContent := do
  match x with
  | .normal s =>
    if s.isEmpty then
      throw "expected a non-empty normal string, but got ''"
    else if GeneralCategory.isLetter s.front then
      return .normal <| s.take 1 ++ ch ++ s.drop 1
    else
      throw s!"diacritics character can only be added after a letter, but got '{s.front}'"
  | .char c => throw s!"expected a non-empty normal string, but got '{c}'"
  | .command _ => throw "expected a non-empty normal string, but got a TeX command"
  | .math _ _ => throw "expected a non-empty normal string, but got a math environment"
  | .braced arr =>
    if h : 0 < arr.size then
      return .braced <| #[← arr[0].addDiacritics ch] ++ (arr.toSubarray.drop 1 |>.toArray)
    else
      throw "expected a non-empty normal string, but got '{}'"

mutual

/-- Convert a TeX content to its original string.
This is not necessarily identical to the original input. -/
partial def toString (x : TexContent) : String :=
  match x with
  | .normal s => s
  | .char c => c.toString
  | .command s => s
  | .math dollar s => dollar ++ s ++ dollar
  | .braced arr => "{" ++ toStringArray arr ++ "}"

partial def toStringArray (arr : Array TexContent) : String :=
  arr.map toString |>.toList |> String.join

end

mutual

/-- Convert a TeX content to plaintext, discarding TeX commands and braces. -/
partial def toPlaintext (x : TexContent) : String :=
  match x with
  | .normal s => s
  | .char c => c.toString
  | .command _ => ""
  | .math dollar s => dollar ++ s ++ dollar
  | .braced arr => toPlaintextArray arr

partial def toPlaintextArray (arr : Array TexContent) : String :=
  arr.map toPlaintext |>.toList |> String.join

end

mutual

/-- Get the first character of the plaintext of a TeX content. -/
partial def getFirstChar (x : TexContent) : Option Char :=
  match x with
  | .normal s => s.get? 0
  | .char c => c
  | .command _ => .none
  | .math dollar _ => dollar.get? 0
  | .braced arr => getFirstCharArray arr

partial def getFirstCharArray (arr : Array TexContent) : Option Char :=
  arr.findSome? getFirstChar

end

mutual

/-- Get the last character of the plaintext of a TeX content. -/
partial def getLastChar (x : TexContent) : Option Char :=
  match x with
  | .normal s => if s.isEmpty then .none else s.back
  | .char c => c
  | .command _ => .none
  | .math dollar _ => dollar.get? 0
  | .braced arr => getLastCharArray arr

partial def getLastCharArray (arr : Array TexContent) : Option Char :=
  arr.findSomeRev? getLastChar

end

mutual

open Std.TreeMap

/-- Convert a TeX content to HTML, represented by an array of `Lean.Xml.Content`.
A few TeX commands can be converted to corresponding HTML. -/
partial def toHtml (x : TexContent) : Array Content :=
  match x with
  | .normal s => #[.Character s]
  | .char c =>
    let ret : Content := match c with
    | '\\' | '$' => .Element ⟨ "span", empty, #[.Character c.toString] ⟩
    | _ => .Character c.toString
    #[ret]
  | .command cmd =>
    let ret : Content := match cmd.trim with
    | "\\\\" => .Element ⟨ "br", empty, #[] ⟩
    | _ => .Element ⟨ "span", empty.insert "style" "color:red;", #[.Character cmd] ⟩
    #[ret]
  | .math dollar s => #[.Character (dollar ++ s ++ dollar)]
  | .braced arr => toHtmlArray arr

partial def toHtmlArray (arr : Array TexContent) (i : Nat := 0)
    (ret : Array Content := #[]) : Array Content :=
  if h : i < arr.size then
    if h' : i + 1 < arr.size then
      if let .command cmd := arr[i] then
        match cmd.trim with
        | "\\url" =>
          let next := arr[i + 1]
          let x : Content := .Element ⟨ "a", empty.insert "href"
            next.toPlaintext, next.toHtml ⟩
          toHtmlArray arr (i + 2) (ret ++ #[x])
        | "\\textrm" =>
          let next := arr[i + 1]
          let x : Content := .Element ⟨ "span", empty.insert "style"
            "font-style: normal; font-weight: normal", next.toHtml ⟩
          toHtmlArray arr (i + 2) (ret ++ #[x])
        | "\\textbf" =>
          let next := arr[i + 1]
          let x : Content := .Element ⟨ "b", empty, next.toHtml ⟩
          toHtmlArray arr (i + 2) (ret ++ #[x])
        | "\\textit" =>
          let next := arr[i + 1]
          let x : Content := .Element ⟨ "i", empty, next.toHtml ⟩
          toHtmlArray arr (i + 2) (ret ++ #[x])
        | "\\emph" =>
          let next := arr[i + 1]
          let x : Content := .Element ⟨ "em", empty, next.toHtml ⟩
          toHtmlArray arr (i + 2) (ret ++ #[x])
        | "\\texttt" =>
          let next := arr[i + 1]
          let x : Content := .Element ⟨ "span", empty.insert "style"
            "font-family: monospace", next.toHtml ⟩
          toHtmlArray arr (i + 2) (ret ++ #[x])
        | "\\textsc" =>
          let next := arr[i + 1]
          let x : Content := .Element ⟨ "span", empty.insert "style"
            "font-variant: small-caps", next.toHtml ⟩
          toHtmlArray arr (i + 2) (ret ++ #[x])
        | _ => toHtmlArray arr (i + 1) (ret ++ arr[i].toHtml)
      else
        toHtmlArray arr (i + 1) (ret ++ arr[i].toHtml)
    else
       toHtmlArray arr (i + 1) (ret ++ arr[i].toHtml)
  else
    ret

end

end TexContent

/-- Match a sequence of space characters and return it. -/
def ws' : Parser String := manyChars <| satisfy fun
  | ' ' | '\t' | '\r' | '\n' => true
  | _ => false

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

/-- Match a TeX command starting with `\`, potentially with trailing whitespaces. -/
def texCommand : Parser String := pchar '\\' *> attempt do
  let s ← manyChars asciiLetter
  if s.isEmpty then
    -- some commands preserve trailing whitespaces
    let c ← any
    match c with
    | '&' | '#' | '{' | '}' | '$' | '_' => return "\\" ++ toString c
    | _ => return "\\" ++ toString c ++ (← ws')
  else if let .some '*' ← peek? then
    skip
    return "\\" ++ s ++ "*" ++ (← ws')
  else
    return "\\" ++ s ++ (← ws')

/-- Similar to `texCommand` but it excludes some commands. -/
def texCommand' (exclude : Array String) : Parser String := attempt do
  let s ← texCommand
  match exclude.find? (· == s.trim) with
  | .some _ => fail s!"'{s.trim}' is not allowed"
  | .none => return s

/-- Match a sequence starting with `{` and ending with `}`. -/
def bracedContent (p : Parser String) : Parser String :=
  pchar '{' *> (("{" ++ · ++ "}") <$> p) <* pchar '}'

partial def manyOptions {α} (p : Parser (Option α)) (acc : Array α := #[]) :
    Parser (Array α) := fun it =>
  match p it with
  | .success it ret =>
    match ret with
    | .some ret => manyOptions p (acc.push ret) it
    | .none => .success it acc
  | .error it err => .error it err

partial def mathContentAux : Parser String := do
  let normalChars : Parser String := many1Chars <| satisfy fun
    | '\\' | '$' | '{' | '}' => false
    | _ => true
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
def mathContent : Parser (Option TexContent) := fun it =>
  let aux (beginning ending : String) : Parser String :=
    pstring beginning *> mathContentAux <* pstring ending
  let substr := it.extract (it.forward 2)
  if substr = "\\[" then
    ((.some <| .math "$$" ·) <$> aux "\\[" "\\]") it
  else if substr = "\\(" then
    ((.some <| .math "$" ·) <$> aux "\\(" "\\)") it
  else if substr = "$$" then
    ((.some <| .math "$$" ·) <$> aux "$$" "$$") it
  else if it.curr = '$' then
    ((.some <| .math "$" ·) <$> aux "$" "$") it
  else
    .success it .none

partial def rawContentAux : Parser String := do
  let normalChars : Parser String := many1Chars <| satisfy fun
    | '\\' | '{' | '}' => false
    | _ => true
  let doOne : Parser (Option String) := fun it =>
    if it.hasNext then
      match it.curr with
      | '{' => (.some <$> bracedContent rawContentAux) it
      | '\\' => (.some <$> texCommand) it
      | '}' => .success it .none
      | _ => (.some <$> normalChars) it
    else
      .success it .none
  return String.join (← manyOptions doOne).toList

/-- Match a TeX command for diacritics, return the processed TeX contents.
Sometimes it needs to read the contents after the command, in this case the `p` is used. -/
def texDiacriticsCommand (p : Parser (Option TexContent)) : Parser (Option TexContent) := do
  let cmd ← texCommand
  -- some special commands
  if cmd.trim = "\\url" then
    let s ← pchar '{' *> rawContentAux <* pchar '}'
    return .some <| .braced #[.command cmd, .braced <| #[.normal s]]
  -- some special characters need to put into `<span>`
  let c : Char := match cmd.trim with
  | "\\$" => '$' | "\\textbackslash" => '\\'
  | _ => ' '
  if c ≠ ' ' then return .some <| .char c
  -- some other characters
  let s : String := match cmd.trim with
  | "\\oe" => "œ" | "\\OE" => "Œ"
  | "\\ae" => "æ" | "\\AE" => "Æ"
  | "\\aa" => "å" | "\\AA" => "Å"
  | "\\o" => "ø" | "\\O" => "Ø"
  | "\\l" => "ł" | "\\L" => "Ł"
  | "\\i" => "ı" | "\\j" => "ȷ"
  | "\\ss" => "\u00DF" | "\\SS" => "\u1E9E"
  | "\\cprime" => "\u02B9"
  | "\\&" => "&" | "\\#" => "#"
  | "\\{" => "{" | "\\}" => "}"
  | "\\_" => "_"
  | "\\" => "\u00A0" -- This should be "\ " but the space is trimmed
  | _ => ""
  if not s.isEmpty then return .some <| .normal s
  -- diacritics characters
  let s : String := match cmd.trim with
  | "\\`" => "\u0300" | "\\'" => "\u0301"
  | "\\^" => "\u0302" | "\\\"" => "\u0308"
  | "\\~" => "\u0303" | "\\=" => "\u0304"
  | "\\." => "\u0307" | "\\u" => "\u0306"
  | "\\v" => "\u030C" | "\\H" => "\u030B"
  | "\\t" => "\u0361" | "\\c" => "\u0327"
  | "\\d" => "\u0323" | "\\b" => "\u0331"
  | "\\k" => "\u0328"
  | _ => ""
  if s.isEmpty then return .some <| .command cmd
  match ← p with
  | .some next =>
    match next.addDiacritics s with
    | .ok ret => return .some ret
    | .error err => fail err
  | .none => fail "expected a non-empty normal string"

/-- Match a segment of TeX content.
The TeX commands for diacritics will be converted into UTF-8 characters.
Other TeX commands are preserved.
Returns `Option.none` if it can't match any and there are no errors. -/
partial def texContent : Parser (Option TexContent) := fun it =>
  let normalChars' : Parser String := many1Chars <| satisfy fun
    | '\\' | '$' | '{' | '}' | ' ' | '\t' | '\r' | '\n' | ',' => false
    | _ => true
  match mathContent it with
  | .success it ret =>
    match ret with
    | .some ret => .success it (.some ret)
    | .none =>
      if it.hasNext then
        match it.curr with
        | ' ' | '\t' | '\r' | '\n' => ((fun _ => .some (.char ' ')) <$> ws) it
        | ',' => .success it.next <| .some <| .char it.curr
        | '\\' => texDiacriticsCommand texContent it
        | '{' => ((.some <| .braced ·) <$> (pchar '{' *> manyOptions texContent <* pchar '}')) it
        | '}' => .success it .none
        | _ => ((.some <| .normal <| replaceChars ·) <$> normalChars') it
      else
        .success it .none
  | .error it err => .error it err

/-- Match a sequence of TeX contents. -/
def texContents : Parser (Array TexContent) := manyOptions texContent

end BibtexQuery.TexDiacritics
