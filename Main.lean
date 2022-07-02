import BibtexQuery.Parser

open Lean

def main : List String → IO Unit
| []       => IO.eprintln "Erreur"
| (s :: _) => do
  let file ← IO.FS.readFile s
  let parsed := BibtexQuery.BibtexFile file.mkIterator
  match parsed with
  | Parsec.ParseResult.success pos res => IO.print $ reprStr res
  | Parsec.ParseResult.error pos err => IO.eprint err
