/-
Copyright (c) 2022 Frédéric Dupuis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Frédéric Dupuis
-/

import Std.Internal.Parsec
import Std.Internal.Parsec.Basic
import Std.Internal.Parsec.String



/-!
# Extra Parsec material

This file contains additional material for the Parsec library, some
of which is modelled after its Haskell counterpart.
-/


open Lean Std.Internal.Parsec.String Std.Internal.Parsec

namespace BibtexQuery.ParsecExtra

def _root_.String.parse? [Inhabited α] (s : String) (p : Parser α) : Option α :=
  match p s.iter with
  | .success _ x => some x
  | .error _ _ => none

def _root_.String.parseDebug [Inhabited α] (s : String) (p : Parser α) : Option (α × String.Pos.Raw) :=
  match p s.iter with
  | .success pos x => some ⟨x, pos.i⟩
  | .error _ _ => none

@[inline]
def noneOf (bad : String) : Parser Char := satisfy (fun z => ¬bad.contains z)

@[inline]
def noneOfStr (bad : String) : Parser String := manyChars (noneOf bad)

@[inline]
def eol : ByteArray.Parser String :=
  ByteArray.pstring "\n\r" <|> ByteArray.pstring "\r\n" <|> ByteArray.pstring "\n"

@[inline]
def maybeSkip (p : Parser α) : Parser Unit := (attempt (p *> pure ())) <|> pure ()

@[inline]
partial def manyCore' (p : Parser α) (acc : List α) : Parser (List α) :=
  (do manyCore' p (acc ++ [← p])) <|> pure acc

@[inline]
def many' (p : Parser α) : Parser (List α) := manyCore' p []

@[inline]
partial def manyStrCore (p : Parser String) (acc : String) : Parser String :=
  (do manyStrCore p (acc ++ (← p))) <|> pure acc

@[inline]
def manyStr (p : Parser String) : Parser String := manyStrCore p ""

@[inline]
partial def sepByCore (pcont : Parser α) (psep : Parser β) (acc : List α) :
    Parser (List α) :=
  attempt (do let _ ← psep; sepByCore pcont psep (acc ++ [← pcont])) <|> pure acc

@[inline]
def sepBy (pcont : Parser α) (psep : Parser β) : Parser (List α) :=
  (do sepByCore pcont psep [← pcont]) <|> pure []

@[inline]
def sepOrEndBy (pcont : Parser α) (psep : Parser β) : Parser (List α) :=
  (do let output ← sepByCore pcont psep [← pcont]; maybeSkip psep; return output) <|> pure []

@[inline]
partial def endByCore (pcont : Parser α) (psep : Parser β) (acc : List α) :
    Parser (List α) :=
  attempt (do let x ← pcont; let _ ← psep; endByCore pcont psep (acc ++ [x])) <|> pure acc

@[inline]
def endBy (pcont : Parser α) (psep : Parser β) : Parser (List α) :=
  (do endByCore pcont psep []) <|> pure []

@[inline]
def alphaNum : Parser Char := attempt do
  let c ← any
  if ('A' ≤ c ∧ c ≤ 'Z') ∨ ('a' ≤ c ∧ c ≤ 'z') ∨ ('0' ≤ c ∧ c ≤ '9') then
    return c
  else fail s!"ASCII alphanumeric character expected"

@[inline]
def asciiLetterToLower : Parser Char := return (← asciiLetter).toLower

@[inline]
def alphaNumToLower : Parser Char := return (← alphaNum).toLower

@[inline]
def asciiWordToLower : Parser String := manyChars asciiLetterToLower

@[inline]
def between (op : Parser α) (cl : Parser α) (mid : Parser β) : Parser β := attempt do
  let _ ← op
  let s ← mid
  let _ ← cl
  return s

@[inline]
def natNum : Parser Nat := attempt do
  let some n := (← manyChars digit).toNat? | fail "Not a natural number"
  return n

def manyCharsUntilWithPrev (test : Option Char → Char → Bool) : Parser String := fun it =>
  let out :=
    it.foldUntil "" fun acc c =>
      let prev : Option Char := if acc == "" then none else acc.back
      if test prev c then none else some (acc ++ c.toString)
  .success out.2 out.1

end BibtexQuery.ParsecExtra
