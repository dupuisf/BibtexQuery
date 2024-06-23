/-
Copyright (c) 2022 Frédéric Dupuis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Frédéric Dupuis
-/

import Lean.Data.Parsec

/-!
# Extra Parsec material

This file contains additional material for the Parsec library, some
of which is modelled after its Haskell counterpart.
-/


open Lean Lean.Parsec

namespace BibtexQuery.ParsecExtra

def _root_.String.parse? [Inhabited α] (s : String) (p : Lean.Parsec α) : Option α :=
  match p s.iter with
  | Lean.Parsec.ParseResult.success _ x => some x
  | Lean.Parsec.ParseResult.error _ _ => none

def _root_.String.parseDebug [Inhabited α] (s : String) (p : Lean.Parsec α) : Option (α × String.Pos) :=
  match p s.iter with
  | Lean.Parsec.ParseResult.success pos x => some ⟨x, pos.i⟩
  | Lean.Parsec.ParseResult.error _ _ => none

@[inline]
def noneOf (bad : String) : Parsec Char := Parsec.satisfy (fun z => ¬bad.contains z)

@[inline]
def noneOfStr (bad : String) : Parsec String := manyChars (noneOf bad)

@[inline]
def eol : Parsec String :=
  Parsec.pstring "\n\r" <|> Parsec.pstring "\r\n" <|> Parsec.pstring "\n"

@[inline]
def maybeSkip (p : Parsec α) : Parsec Unit := (attempt (p *> pure ())) <|> pure ()

@[inline]
partial def manyCore' (p : Parsec α) (acc : List α) : Parsec (List α) :=
  (do manyCore' p (acc ++ [← p])) <|> pure acc

@[inline]
def many' (p : Parsec α) : Parsec (List α) := manyCore' p []

@[inline]
partial def manyStrCore (p : Parsec String) (acc : String) : Parsec String :=
  (do manyStrCore p (acc ++ (← p))) <|> pure acc

@[inline]
def manyStr (p : Parsec String) : Parsec String := manyStrCore p ""

@[inline]
partial def sepByCore (pcont : Parsec α) (psep : Parsec β) (acc : List α) :
    Parsec (List α) :=
  attempt (do let _ ← psep; sepByCore pcont psep (acc ++ [← pcont])) <|> pure acc

@[inline]
def sepBy (pcont : Parsec α) (psep : Parsec β) : Parsec (List α) :=
  (do sepByCore pcont psep [← pcont]) <|> pure []

@[inline]
def sepOrEndBy (pcont : Parsec α) (psep : Parsec β) : Parsec (List α) :=
  (do let output ← sepByCore pcont psep [← pcont]; maybeSkip psep; return output) <|> pure []

@[inline]
partial def endByCore (pcont : Parsec α) (psep : Parsec β) (acc : List α) :
    Parsec (List α) :=
  attempt (do let x ← pcont; let _ ← psep; endByCore pcont psep (acc ++ [x])) <|> pure acc

@[inline]
def endBy (pcont : Parsec α) (psep : Parsec β) : Parsec (List α) :=
  (do endByCore pcont psep []) <|> pure []

@[inline]
def alphaNum : Parsec Char := attempt do
  let c ← anyChar
  if ('A' ≤ c ∧ c ≤ 'Z') ∨ ('a' ≤ c ∧ c ≤ 'z') ∨ ('0' ≤ c ∧ c ≤ '9') then
    return c
  else fail s!"ASCII alphanumeric character expected"

@[inline]
def asciiLetterToLower : Parsec Char := return (← asciiLetter).toLower

@[inline]
def alphaNumToLower : Parsec Char := return (← alphaNum).toLower

@[inline]
def asciiWordToLower : Parsec String := manyChars asciiLetterToLower

@[inline]
def between (op : Parsec α) (cl : Parsec α) (mid : Parsec β) : Parsec β := attempt do
  let _ ← op
  let s ← mid
  let _ ← cl
  return s

@[inline]
def natNum : Parsec Nat := attempt do
  let some n := (← manyChars digit).toNat? | fail "Not a natural number"
  return n

def manyCharsUntilWithPrev (test : Option Char → Char → Bool) : Parsec String := fun it =>
  let out :=
    it.foldUntil "" fun acc c =>
      let prev : Option Char := if acc == "" then none else acc.back
      if test prev c then none else some (acc ++ c.toString)
  .success out.2 out.1

end BibtexQuery.ParsecExtra
