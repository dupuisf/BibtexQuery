/-
Copyright (c) 2024 Jz Pan. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Jz Pan
-/

import BibtexQuery.TexDiacritics
import UnicodeBasic

/-!

# Bibtex name processing

This file contains functions for bibtex name processing.
Unless stated otherwise, the input string should have no TeX commands
and math equations. Braces are allowed.

An exception is the main function `processNames` which inputs a bibtex name string,
allowing TeX diacritics commands, return an array of `BibtexName`.
The math equations are still not allowed.

-/

open Lean Parsec Unicode

namespace BibtexQuery.TexDiacritics

partial def getNameBracedAux : Parsec String := do
  let doOne : Parsec (Option String) := fun it =>
    if it.hasNext then
      match it.curr with
      | '{' => (.some <$> bracedContent getNameBracedAux) it
      | '}' => .success it .none
      | _ => (.some <$> normalChars) it
    else
      .success it .none
  return String.join (← manyOptions doOne).toList

def normalCharWithoutSpaceOrComma : Parsec Char := satisfy fun c =>
  match c with
  | '\\' | '$' | '{' | '}' | ' ' | '\t' | '\r' | '\n' | ',' => false
  | _ => true

def normalCharsWithoutSpaceOrComma : Parsec String := do
  let s ← many1Chars normalCharWithoutSpaceOrComma
  pure <| replaceChars s

/-- Input a bibtex name string without TeX commands
and math equations, split the string by " " and ",". -/
def getNameAux : Parsec (Array String) := do
  let doOne' : Parsec (Option String) := fun it =>
    if it.hasNext then
      match it.curr with
      | '{' => (.some <$> bracedContent getNameBracedAux) it
      | '}' | ' ' | '\t' | '\r' | '\n' | ',' => .success it .none
      | _ => (.some <$> normalCharsWithoutSpaceOrComma) it
    else
      .success it .none
  let doOne : Parsec (Option String) := fun it =>
    if it.hasNext then
      match it.curr with
      | '}' => .success it .none
      | ' ' | '\t' | '\r' | '\n' => (.some <$> ws') it
      | ',' => .success it.next ","
      | _ => ((.some <| String.join ·.toList) <$> manyOptions doOne') it
    else
      .success it .none
  let arr ← manyOptions doOne
  return arr.filterMap fun s =>
    let t := s.trim
    if t.isEmpty then .none else .some t

/-- Input a name string already split by spaces and comma,
return `(Firstname, Lastname)`.
The braces in the name are preserved. The logic is:

1. If there is a "," in the array, then the items before the first "," are the last name,
   and the items after the first "," are the first name.
2. Otherwise, if the last item begins with "{" and ends with "}", then it is the last name
   (after removing the outmost braces), the remaining items are the first name.
3. Otherwise, if there is an item that begins with a lowercase letter, then the items before
   the first of such item are the first name, the remaining items are the last name.
4. Otherwise, the last item is the last name, the remaining items are the first name.
-/
def getName (arr : Array String) : String × String :=
  if arr.isEmpty then
    ("", "")
  else
    let join' (arr : Subarray String) : String := arr.foldl (fun acc s =>
      acc ++ (if acc.isEmpty || s.isEmpty || s == "," then "" else " ") ++ s) ""
    match arr.getIdx? "," with
    | .some n =>
      (arr.toSubarray.drop (n + 1) |> join', arr.toSubarray.take n |> join')
    | .none =>
      let s := arr.get! (arr.size - 1)
      if s.startsWith "{" && s.endsWith "}" then
        (arr.toSubarray.take (arr.size - 1) |> join',
          s.toSubstring.drop 1 |>.dropRight 1 |>.toString)
      else
        match arr.findIdx? fun s => isLowercase s.front with
        | .some i =>
          (arr.toSubarray.take i |> join', arr.toSubarray.drop i |> join')
        | .none =>
          (arr.toSubarray.take (arr.size - 1) |> join', s)

/-- Input a bibtex name string without TeX commands
and math equations, return an array of `(Firstname, Lastname)`.
The braces in the name are preserevd. -/
def getNames : Parsec (Array (String × String)) := do
  let arr ← getNameAux
  let arr2 : Array (Array String) := arr.foldl (fun acc s =>
    if s = "and" then
      acc.push #[]
    else
      acc.modify (acc.size - 1) (Array.push · s)) #[#[]]
  return arr2.filterMap fun arr =>
    let ret := getName arr
    if ret.1.isEmpty && ret.2.isEmpty then .none else .some ret

/-- Strip diacritics from a character. -/
def stripDiacritics (c : Char) : Char :=
  match c with
  | 'œ' => 'o' | 'Œ' => 'O'
  | 'æ' => 'a' | 'Æ' => 'A'
  | 'å' => 'a' | 'Å' => 'A'
  | 'ø' => 'o' | 'Ø' => 'O'
  | 'ł' => 'l' | 'Ł' => 'L'
  | 'ı' => 'i'
  | 'ȷ' => 'j'
  | '\u00DF' => 's' | '\u1E9E' => 'S'
  | _ =>
    let s := getCanonicalDecomposition c
    s.get? (s.find fun c => (getUnicodeData c).canonicalCombiningClass == 0) |>.getD c

/-- Strip diacritics from a string. -/
def stripDiacriticsFromString (s : String) : String :=
  s.toList.toArray.map stripDiacritics |>.filter (not <| GeneralCategory.isMark ·)
    |>.toList |> String.mk

/-- Get the array of alphabets of a string after stripping diacritics. -/
def getAlphabets (s : String) : Array Char :=
  s.toList.toArray.map stripDiacritics |>.filter isAlphabetic

/-- Check if a string is an upper case Roman numerals.
It does not check the validity of the number, for example, it accepts `IXIX`. -/
def isUppercaseRomanNumerals (s : String) : Bool :=
  not s.isEmpty && s.all fun c =>
    match c with
    | 'I' | 'V' | 'X' | 'L' | 'C' | 'D' | 'M' => true
    | _ => false

/-- Input a last name string without TeX commands, braces
and math equations, already split by spaces and comma,
return `(oneLetterAbbr, threeLetterAbbr)` of the last name.
Note that they are not necessarily of one-letter and
three-letter; they are if the last name contains no spaces and with only one uppercase letter.
The logic is:

First, if there are more than one items, remove all items which are upper case Roman numerals.
For example, this removes "III" in "Gerth III".

1. If the number of items is not exactly one, then both of the abbreviations are the concatenation
   of the first alphabets of each item.
2. Otherwise, if there are exactly two uppercase alphabets and there is exactly one alphabet
   between them, then both of the abbreviations are these three alphabets.
   For example, `McCrimmon => McC`.
3. Otherwise, if there are at least two uppercase alphabets, then both of the abbreviations are
   the concatenation of uppercase alphabets.
   For example, `Heath-Brown => HB`.
4. Otherwise, the abbreviations are the first one and three alphabets of the last name,
   respectively.
-/
def getLastNameAbbr (arr : Array String) : String × String :=
  let arr := if arr.size ≤ 1 then arr else arr.filter (not <| isUppercaseRomanNumerals ·)
  match arr with
  | #[] => ("", "")
  | #[s] =>
    let s := getAlphabets s
    let arr : Array Nat := s.zipWithIndex.filterMap fun x =>
      if isUppercase x.1 then .some x.2 else .none
    if arr.size = 2 ∧ arr[0]! + 2 = arr[1]! then
      let s := s.toSubarray.drop arr[0]! |>.take 3 |>.toArray.toList |> String.mk
      (s, s)
    else if arr.size ≥ 2 then
      let s := arr.map s.get! |>.toList |> String.mk
      (s, s)
    else
      let s := String.mk s.toList
      (s.take 1, s.take 3)
  | _ =>
    let s := arr.filterMap (getAlphabets · |>.get? 0) |>.toList |> String.mk
    (s, s)

/-- Represents the name of a person in bibtex author field. -/
structure BibtexName where
  /-- The first name without TeX commands and braces. -/
  firstName : String
  /-- The last name without TeX commands and braces. -/
  lastName : String
  /-- The first name without TeX commands, braces and diacritics,
  all letters converted to uppercase. -/
  firstNameWithoutDiacritics : String
  /-- The last name without TeX commands, braces and diacritics,
  all letters converted to uppercase. -/
  lastNameWithoutDiacritics : String
  /-- The one-letter abbreviation of the last name, which is used if a bibitem has at least
  two authors. Note that this is not necessarily of one-letter;
  it is if the last name contains no spaces and with only one uppercase letter. -/
  oneLetterAbbr : String
  /-- The three-letter abbreviation of the last name, which is used if a bibitem has exactly
  one author. Note that this is not necessarily of three-letter;
  it is if the last name contains no spaces and with only one uppercase letter. -/
  threeLetterAbbr : String
  deriving Repr

/-- Process the first name and last name without TeX commands
and math equations, remove all braces in them, and produce abbreviations of the last name. -/
def processName (s : String × String) : Except String BibtexName :=
  let removeBraces' (s : String) : Except String String :=
    match removeBraces s.iter with
    | .success _ s => .ok s
    | .error it err => .error s!"failed to run removeBraces on '{it.1}' at pos {it.2}: {err}"
  match removeBraces' s.1 with
  | .ok firstName =>
    match removeBraces' s.2 with
    | .ok lastName =>
      match getNameAux s.2.iter with
      | .success _ arr =>
        match arr.mapM removeBraces' with
        | .ok arr =>
          let abbr := getLastNameAbbr <| arr.filter (not ·.trim.isEmpty)
          .ok {
            firstName := firstName
            lastName := lastName
            firstNameWithoutDiacritics := stripDiacriticsFromString firstName |>.map getUpperChar
            lastNameWithoutDiacritics := stripDiacriticsFromString lastName |>.map getUpperChar
            oneLetterAbbr := abbr.1
            threeLetterAbbr := abbr.2
          }
        | .error err => .error err
      | .error it err => .error s!"failed to run getNameAux on '{it.1}' at pos {it.2}: {err}"
    | .error err => .error err
  | .error err => .error err

/-- Input a bibtex name string without math equations, return an array of `BibtexName`. -/
def processNames (s : String) : Except String (Array BibtexName) :=
  match texDiacritics s.iter with
  | .success _ s =>
    match getNames s.iter with
    | .success _ arr => arr.mapM processName
    | .error it err => .error s!"failed to run getNames on '{it.1}' at pos {it.2}: {err}"
  | .error it err => .error s!"failed to run texDiacritics on '{it.1}' at pos {it.2}: {err}"

end BibtexQuery.TexDiacritics
