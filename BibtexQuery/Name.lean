/-
Copyright (c) 2024 Jz Pan. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Jz Pan
-/

import BibtexQuery.TexDiacritics
import UnicodeBasic
import Std.Internal.Parsec
import Std.Internal.Parsec.String


/-!

# Bibtex name processing

This file contains functions for bibtex name processing.

The main function is `processNames` which inputs an array of `TexContent`,
return an array of `BibtexName`.

-/

open Lean Unicode BibtexQuery.TexDiacritics

namespace BibtexQuery.Name

/-- Input an array of `TexContent`, split them by " " and ",". -/
def getNameAux (arr : Array TexContent) : Array (Array TexContent) :=
  arr.foldl (fun acc s =>
    match s with
    | .char ' ' =>
      acc.push #[]
    | .char ',' =>
      acc.push #[s] |>.push #[]
    | _ =>
      acc.modify (acc.size - 1) (Array.push · s)) #[#[]] |>.filter (not ·.isEmpty)

/-- Input a name string already split by " " and ",", return `(Firstname, Lastname)`.
The braces in the name are preserved. The logic is:

1. If there is a "," in the array, then the items before the first "," are the last name,
   and the items after the first "," are the first name.
2. Otherwise, if the last item begins with "{" and ends with "}", then it is the last name,
   the remaining items are the first name.
3. Otherwise, if there is an item that begins with a lowercase letter, then the items before
   the first of such item are the first name, the remaining items are the last name.
4. Otherwise, the last item is the last name, the remaining items are the first name.
-/
def getName (arr : Array (Array TexContent)) :
    Array (Array TexContent) × Array (Array TexContent) :=
  match arr.findIdx? (fun
    | #[.char ','] => true
    | _ => false) with
  | .some n =>
    (arr.toSubarray.drop (n + 1) |>.toArray, arr.toSubarray.take n |>.toArray)
  | .none =>
    let i := match arr.back? with
    | .some #[.braced _] => arr.size - 1
    | _ => arr.findIdx? (fun s => s.findSome? TexContent.getFirstChar
      |>.getD ' ' |> isLowercase) |>.getD (arr.size - 1)
    (arr.toSubarray.take i |>.toArray, arr.toSubarray.drop i |>.toArray)

/-- Input an array of `TexContent`, return an array of `(Firstname, Lastname)`.
The braces in the name are preserevd. -/
def getNames (arr : Array TexContent) :
    Array (Array (Array TexContent) × Array (Array TexContent)) :=
  let arr := getNameAux arr
  let arr2 : Array (Array (Array TexContent)) := arr.foldl (fun acc s =>
    match s with
    | #[.normal "and"] => acc.push #[]
    | _ => acc.modify (acc.size - 1) (Array.push · s)) #[#[]]
  arr2.filterMap fun x =>
    let ret := getName x
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
    s.get? (s.find fun c => getCanonicalCombiningClass c == 0) |>.getD c

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
    let arr : Array Nat := s.zipIdx.filterMap fun x =>
      if isUppercase x.1 then .some x.2 else .none
    if arr.size = 2 ∧ arr[0]! + 2 = arr[1]! then
      let s := s.toSubarray.drop arr[0]! |>.take 3 |>.toArray.toList |> String.mk
      (s, s)
    else if arr.size ≥ 2 then
      let s := arr.map (s[·]!) |>.toList |> String.mk
      (s, s)
    else
      let s := String.mk s.toList
      (s.take 1, s.take 3)
  | _ =>
    let s := arr.filterMap (getAlphabets · |> (·[0]?)) |>.toList |> String.mk
    (s, s)

/-- Represents the name of a person in bibtex author field. -/
structure BibtexName where
  /-- The first name. -/
  firstName : Array (Array TexContent)
  /-- The last name. -/
  lastName : Array (Array TexContent)
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

namespace BibtexName

/-- Convert a `BibtexName` to `Firstname Lastname` with TeX commands and braces.
This is not necessarily identical to the original input. -/
def toString (x : BibtexName) : String :=
  let g (arr : Array (Array TexContent)) : String :=
    " ".intercalate (arr.map TexContent.toStringArray |>.toList)
  g <| x.firstName ++ x.lastName

def toStringArray (arr : Array BibtexName) : String :=
  " and ".intercalate (arr.map toString |>.filter (not ·.isEmpty) |>.toList)

/-- Convert a `BibtexName` to `Firstname Lastname` without TeX commands and braces. -/
def toPlaintext (x : BibtexName) : String :=
  let g (arr : Array (Array TexContent)) : String :=
    " ".intercalate (arr.map TexContent.toPlaintextArray |>.toList)
  g <| x.firstName ++ x.lastName

def toPlaintextArray (arr : Array BibtexName) : String :=
  ", ".intercalate (arr.map toString |>.filter (not ·.isEmpty) |>.toList)

/-- Convert a `BibtexName` to `Firstname Lastname` of HTML form. -/
def toHtml (x : BibtexName) : Array Xml.Content :=
  let g (arr : Array (Array TexContent)) : Array Xml.Content :=
    arr.foldl (fun acc s =>
      let t := TexContent.toHtmlArray s
      acc ++ (if acc.isEmpty || t.isEmpty then #[] else #[.Character " "]) ++ t) #[]
  g <| x.firstName ++ x.lastName

def toHtmlArray (arr : Array BibtexName) : Array Xml.Content :=
  arr.foldl (fun acc s =>
    let t := s.toHtml
    acc ++ (if acc.isEmpty || t.isEmpty then #[] else #[.Character ", "]) ++ t) #[]

end BibtexName

/-- Process the first name and last name, produce abbreviations of the last name. -/
def processName (s : Array (Array TexContent) × Array (Array TexContent)) : BibtexName :=
  let g (arr : Array (Array TexContent)) : String :=
    " ".intercalate (arr.map TexContent.toPlaintextArray |>.toList)
  let abbr := getLastNameAbbr <| s.2.map TexContent.toPlaintextArray
  {
    firstName := s.1
    lastName := s.2
    firstNameWithoutDiacritics := stripDiacriticsFromString (g s.1) |>.map getUpperChar
    lastNameWithoutDiacritics := stripDiacriticsFromString (g s.2) |>.map getUpperChar
    oneLetterAbbr := abbr.1
    threeLetterAbbr := abbr.2
  }

/-- Input an array of `TexContent`, return an array of `BibtexName`. -/
def processNames (arr: Array TexContent) : Array BibtexName :=
  arr |> getNames |>.map processName

end BibtexQuery.Name
