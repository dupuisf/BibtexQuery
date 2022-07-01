import Lean.Data.Parsec

open Lean

namespace Lean.Parsec

def noneOf (bad : String) : Parsec Char := Parsec.satisfy (fun z => ¬bad.contains z)

--def Lean.Parsec.line : Parsec String := Parsec.manyChars $ Parsec.noneOf '\n'

def eol : Parsec String := 
  Parsec.pstring "\n\r" <|> Parsec.pstring "\r\n" <|> Parsec.pstring "\n"

partial def manyCore' (p : Parsec α) (acc : List α) : Parsec (List α) :=
(do manyCore' p (acc ++ [←p])) <|> pure acc

def many' (p : Parsec α) : Parsec (List α) := Parsec.manyCore' p []

partial def sepByCore (pcont : Parsec α) (psep : Parsec β) (acc : List α) : 
  Parsec (List α) :=
(do let _ ← psep; sepByCore pcont psep (acc ++ [←pcont])) <|> pure acc

def sepBy (pcont : Parsec α) (psep : Parsec β) : Parsec (List α) :=
do Parsec.sepByCore pcont psep [←pcont]

partial def fuckme (p : Parsec α) : Parsec α := attempt p <|> fail "fukcme"

partial def sepByCore' (pcont : Parsec α) (psep : Parsec β) (acc : List α) : 
  Parsec (List α) := 
attempt (do let _ ← psep; sepByCore' pcont psep (acc ++ [←pcont]))
<|> (do  let _ ← attempt psep; return acc)
<|> pure acc

def sepBy' (pcont : Parsec α) (psep : Parsec β) : Parsec (List α) :=
do Parsec.sepByCore' pcont psep [←pcont]

partial def endByCore (pcont : Parsec α) (psep : Parsec β) (acc : List α) :
  Parsec (List α) :=
(do let x ← pcont; let _ ← psep; endByCore pcont psep (acc ++ [x])) <|> pure acc

def endBy (pcont : Parsec α) (psep : Parsec β) : Parsec (List α) :=
(do Parsec.endByCore pcont psep [])

def asciiLetterToLower : Parsec Char := do return (←asciiLetter).toLower

def asciiWordToLower : Parsec String := manyChars asciiLetterToLower


end Lean.Parsec