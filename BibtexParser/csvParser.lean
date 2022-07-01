import Lean4Bibtex.ParsecExtra

open Lean

namespace csvParser1

def cellContent : Parsec String := Parsec.manyChars (Lean.Parsec.noneOf ",\n")

mutual 
  partial def cells : Parsec (List String) := do 
    let first ← cellContent
    let next ← remainingCells
    return ([first] ++ next)

  partial def remainingCells : Parsec (List String) := do 
    match (←Parsec.peek?) with
    | ',' => Parsec.skipChar ','; cells
    | _   => return []
end

def line : Parsec (List String) := do 
  let result ← cells
  let _ ← Parsec.eol 
  return result

def csvFile : Parsec (List (List String)) := do 
  let result ← Parsec.many' line
  Parsec.eof
  return result

--#eval line (String.mkIterator "OK, voici un test¬")

end csvParser1

namespace csvParser2

def cell := Parsec.manyChars (Parsec.noneOf ",\n")
def line := Parsec.sepBy cell (Parsec.pchar ',')
def csvLine := Parsec.endBy line Parsec.eol

#eval csvLine (String.mkIterator "OK, voici un test\r\nun, autre, test\n\r")

end csvParser2