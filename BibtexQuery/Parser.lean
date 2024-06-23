/-
Copyright (c) 2022 Frédéric Dupuis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Frédéric Dupuis
-/

import BibtexQuery.ParsecExtra
import BibtexQuery.Entry

/-!
# Bibtex Parser

This file contains a parser for the Bibtex format. Note that currently, only a subset of the official
Bibtex format is supported; features such as predefined strings and concatenation using `#` are not
supported.
-/

open Lean Parsec BibtexQuery.ParsecExtra

namespace BibtexQuery.Parser

/-- The name of the bibtex entry (i.e. what goes in the cite command). -/
def name : Parsec String := attempt do
  let firstChar ← Parsec.asciiLetter
  let remainder ← manyChars <| (alphaNum <|> pchar ':' <|> pchar '-' <|> pchar '_')
  return firstChar.toString ++ remainder

/-- "article", "book", etc -/
def category : Parsec String := attempt do skipChar '@'; asciiWordToLower

partial def bracedContentTail (acc : String) : Parsec String := attempt do
  let c ← anyChar
  if c = '{' then
    let s ← bracedContentTail ""
    bracedContentTail (acc ++ "{" ++ s)
  else
    if c = '}' then return acc ++ "}"
    else
      bracedContentTail (acc ++ c.toString)

def bracedContent : Parsec String := attempt do
  skipChar '{'
  let s ← bracedContentTail ""
  return s.dropRight 1

def quotedContent : Parsec String := attempt do
  skipChar '"'
  let s ← manyCharsUntilWithPrev fun | (some '\\'), '"' => false | _, '"' => true | _, _ => false
  skipChar '"'
  return (s.replace "\n" "").replace "\r" ""

def month : Parsec String := attempt do
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

/-- The content field of a tag. -/
def tagContent : Parsec String := attempt do
  let c ← peek!
  if c.isDigit then manyChars digit else
    if c.isAlpha then month else
      match c with
      | '"' => quotedContent
      | '{' => bracedContent
      | _   => fail "Tag content expected"

/-- i.e. journal = {Journal of Musical Deontology} -/
def tag : Parsec Tag := attempt do
  let tagName ← manyChars (alphaNumToLower <|> pchar '_' <|> pchar '-')
  ws; skipChar '='; ws
  let tagContent ← tagContent
  return { name := tagName, content := tagContent }

def outsideEntry : Parsec Unit := attempt do
  let _ ← manyChars <| noneOf "@"

/-- A Bibtex entry. TODO deal with "preamble" etc. -/
def entry : Parsec Entry := attempt do
  outsideEntry
  let typeOfEntry ← category
  ws; skipChar '{'; ws
  let nom ← name
  skipChar ','; ws
  let t : List Tag ← sepOrEndBy tag (do ws; skipChar ','; ws)
  ws; skipChar '}'; ws
  return Entry.normalType typeOfEntry nom t

def bibtexFile : Parsec (List Entry) := many' entry

--#eval "auTHOr23:z  ".parseDebug name
--#eval "auTHOr23:z".parseDebug name
--#eval "@ARTICLE ".parseDebug category
--#eval "@ARtiCLE".parseDebug category
--#eval "auTHOr =   \n{Dès Noël où un zéphyr haï\n me vêt de glaçons würmiens, je dîne d'exquis rôtis de bœuf au kir à l'aÿ d'âge mûr}".parseDebug tag
--#eval "auTHOr = \"Test\"".parseDebug tag
--#eval "journal = {Journal of Musical\n Deontology}".parseDebug tag
--#eval "year = 2022".parseDebug tag
--#eval "Bdsk-Url-1 = {https://doi.org/10.1007/s00220-020-03839-5}".parseDebug tag
--#eval "year = 2022,\n author = {Frédéric Dupuis},".parseDebug (sepOrEndBy tag (do ws; skipChar ','; ws))
--#eval "@article{bla23,\n year = 2022,\n author = {Frédéric Dupuis}\n}\n".parseDebug entry
--#eval "\"Bachem, Achim and Korte, Bernhard and Gr{\\\"o}tschel\"".parseDebug quotedContent
--#eval "@article{bla23,\n year = 2022,\n author = \"Bachem, Achim and Korte, Bernhard and Gr{\"o}tschel\"\n}\n".parseDebug entry

end BibtexQuery.Parser
