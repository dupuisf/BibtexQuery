import BibtexQuery.ParsecExtra

open Lean Parsec

namespace BibtexQuery

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
def BibtexQuery.name : Parsec String := do
  let firstChar ← Parsec.asciiLetter
  let reste ← manyChars $ (asciiLetter <|> digit <|> pchar ':' <|> pchar '-')
  return firstChar.toString ++ reste

/-- "article", "book", etc -/
def BibtexQuery.class : Parsec String := do skipChar '@'; asciiWordToLower

def BibtexQuery.quotedContent : Parsec String := do 
  skipChar '"'
  let s ← manyChars $ noneOf "\""
  skipChar '"'
  return (s.replace "\n" "").replace "\r" ""

def BibtexQuery.bracedContent : Parsec String := do 
  skipChar '{'
  let s ← manyChars $ noneOf "}"
  skipChar '}'
  return (s.replace "\n" "").replace "\r" ""

def BibtexQuery.month : Parsec String := do 
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
def BibtexQuery.tagContent : Parsec String := do 
  let c ← peek!
  if c.isDigit then manyChars digit else
    if c.isAlpha then BibtexQuery.month else
      match c with
      | '"' => BibtexQuery.quotedContent
      | '{' => BibtexQuery.bracedContent
      | _   => fail "Tag content expected"

/-- i.e. journal = {Journal of Musical Deontology} -/
def BibtexQuery.tag : Parsec Tag := do 
  let tagName ← asciiWordToLower
  ws; skipChar '='; ws
  let tagContent ← BibtexQuery.tagContent
  return { Name := tagName, Content := tagContent }

def BibtexQuery.outsideEntry : Parsec Unit := do let _ ← manyChars $ noneOf "@"

/-- A Bibtex entry. TODO deal with "preamble" and all that crap. -/
def BibtexQuery.entry : Parsec Entry := do 
  BibtexQuery.outsideEntry
  let typeOfEntry ← BibtexQuery.class
  ws; skipChar '{'; ws
  let nom ← BibtexQuery.name
  skipChar ','; ws
  let t : List Tag ← sepBy' BibtexQuery.tag (do ws; skipChar ','; ws)
  ws; skipChar '}'; ws
  return Entry.NormalType typeOfEntry nom t

def BibtexFile : Parsec (List Entry) := many' BibtexQuery.entry

--#eval BibtexQuery.name "auTHOr23:z  ".mkIterator
--#eval BibtexQuery.class "@ARTICLE ".mkIterator
--#eval BibtexQuery.tag "auTHOr =   \n{Dès Noël où un zéphyr haï\n me vêt de glaçons würmiens, je dîne d'exquis rôtis de bœuf au kir à l'aÿ d'âge mûr}".mkIterator
--#eval BibtexQuery.tag "auTHOr = \"Test\"".mkIterator
--#eval BibtexQuery.tag "journal = {Journal of Musical\n Deontology}".mkIterator
--#eval BibtexQuery.tag "year = 2022".mkIterator
--#eval BibtexQuery.entry "  @article{bla23,\n year = 2022,\n author = {Frédéric Dupuis}\n}\n".mkIterator
--#eval (Parsec.sepBy (manyChars $ Parsec.noneOf ",") (skipChar ',')) "bla, foo,".mkIterator

--#eval (sepBy' asciiWordToLower (do ws; skipChar ','; ws)) "bla, foo, baz, ".mkIterator


end BibtexQuery

open Lean
