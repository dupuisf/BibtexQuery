import Lean4Bibtex.ParsecExtra

open Lean Parsec

namespace Lean4Bibtex

/-- i.e. authors = "Binne, Chose and Truc, Machin" -/
structure Tag where
  Name : String
  Content : String
deriving Repr

inductive Entry where
  | NormalType (Class : String) (Name : String) (Tags : List Tag)
  | StringType (Content : String)
  | PreambleType (Content : String)
  | CommentType
deriving Repr

/-- The name of the bibtex entry (i.e. what goes in the cite command). -/
def BibtexParser.name : Parsec String := do
  let firstChar ← Parsec.asciiLetter
  let reste ← manyChars $ (asciiLetter <|> digit <|> pchar ':' <|> pchar '-')
  return firstChar.toString ++ reste

/-- "article", "book", etc -/
def BibtexParser.class : Parsec String := do skipChar '@'; asciiWordToLower

def BibtexParser.quotedContent : Parsec String := do 
  skipChar '"'
  let s ← manyChars $ noneOf "\""
  skipChar '"'
  return (s.replace "\n" "").replace "\r" ""

def BibtexParser.bracedContent : Parsec String := do 
  skipChar '{'
  let s ← manyChars $ noneOf "}"
  skipChar '}'
  return (s.replace "\n" "").replace "\r" ""

def BibtexParser.month : Parsec String := do 
  let s ← asciiWordToLower
  match s with
  | "jan" => return s
  | "feb" => return s
  | "mar" => return s
  | "apr" => return s
  | "may" => return s
  | "jun" => return s
  | "jul" => return s
  | "aug" => return s
  | "sep" => return s
  | "oct" => return s
  | "nov" => return s
  | "dec" => return s
  | _     => fail "Not a valid month"

/-- The content field of a tag. TODO: deal with months. -/
def BibtexParser.tagContent : Parsec String := do 
  let c ← peek!
  if c.isDigit then manyChars digit else
    if c.isAlpha then BibtexParser.month else
      match c with
      | '"' => BibtexParser.quotedContent
      | '{' => BibtexParser.bracedContent
      | _   => fail "Tag content expected"

/-- i.e. journal = {Journal of Musical Deontology} -/
def BibtexParser.tag : Parsec Tag := do 
  let tagName ← asciiWordToLower
  ws; skipChar '='; ws
  let tagContent ← BibtexParser.tagContent
  return { Name := tagName, Content := tagContent }

def BibtexParser.outsideEntry : Parsec Unit := do let _ ← manyChars $ noneOf "@"

/-- A Bibtex entry. TODO deal with "preamble" and all that crap. -/
def BibtexParser.entry : Parsec Entry := do 
  BibtexParser.outsideEntry
  let typeOfEntry ← BibtexParser.class
  ws; skipChar '{'; ws
  let nom ← BibtexParser.name
  skipChar ','; ws
  let t : List Tag ← sepBy' BibtexParser.tag (do ws; skipChar ','; ws)
  ws; skipChar '}'; ws
  return Entry.NormalType typeOfEntry nom t

def BibtexFile : Parsec (List Entry) := many' BibtexParser.entry

--#eval BibtexParser.name "auTHOr23:z  ".mkIterator
--#eval BibtexParser.class "@ARTICLE ".mkIterator
--#eval BibtexParser.tag "auTHOr =   \n{Dès Noël où un zéphyr haï\n me vêt de glaçons würmiens, je dîne d'exquis rôtis de bœuf au kir à l'aÿ d'âge mûr}".mkIterator
--#eval BibtexParser.tag "auTHOr = \"Test\"".mkIterator
--#eval BibtexParser.tag "journal = {Journal of Musical\n Deontology}".mkIterator
--#eval BibtexParser.tag "year = 2022".mkIterator
--#eval BibtexParser.entry "  @article{bla23,\n year = 2022,\n author = {Frédéric Dupuis}\n}\n".mkIterator
--#eval (Parsec.sepBy (manyChars $ Parsec.noneOf ",") (skipChar ',')) "bla, foo,".mkIterator

--#eval (sepBy' asciiWordToLower (do ws; skipChar ','; ws)) "bla, foo, baz, ".mkIterator


end Lean4Bibtex

open Lean