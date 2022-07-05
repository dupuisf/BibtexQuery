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

open Lean Parsec

namespace BibtexQuery.Parser

/-- The name of the bibtex entry (i.e. what goes in the cite command). -/
def name : Parsec String := do
  let firstChar ← Parsec.asciiLetter
  let reste ← manyChars $ (asciiLetter <|> digit <|> pchar ':' <|> pchar '-')
  return firstChar.toString ++ reste

/-- "article", "book", etc -/
def classe : Parsec String := do skipChar '@'; asciiWordToLower

partial def bracedContentAux (acc : String) : Parsec String :=
(do let c ← anyChar
    if c = '{' then
      let s ← bracedContentAux ""
      bracedContentAux (acc ++ s)
    else
      if c = '}' then return acc
      else bracedContentAux (acc ++ c.toString))

def bracedContent : Parsec String := do 
  skipChar '{'
  return ((←bracedContentAux "").replace "\n" "").replace "\r" ""

def quotedContent : Parsec String := do 
  skipChar '"'
  let s ← manyChars $ noneOf "\""
  skipChar '"'
  return (s.replace "\n" "").replace "\r" ""

def month : Parsec String := do 
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
def tagContent : Parsec String := do 
  let c ← peek!
  if c.isDigit then manyChars digit else
    if c.isAlpha then month else
      match c with
      | '"' => quotedContent
      | '{' => bracedContent
      | _   => fail "Tag content expected"

/-- i.e. journal = {Journal of Musical Deontology} -/
def tag : Parsec Tag := do 
  --let tagName ← asciiWordToLower
  let tagName ← manyChars (asciiLetterToLower <|> pchar '_')
  ws; skipChar '='; ws
  let tagContent ← tagContent
  return { Name := tagName, Content := tagContent }

def outsideEntry : Parsec Unit := do let _ ← manyChars $ noneOf "@"

/-- A Bibtex entry. TODO deal with "preamble" and all that crap. -/
def entry : Parsec Entry := do 
  outsideEntry
  let typeOfEntry ← classe
  ws; skipChar '{'; ws
  let nom ← name
  skipChar ','; ws
  let t : List Tag ← sepBy' tag (do ws; skipChar ','; ws)
  ws; skipChar '}'; ws
  return Entry.NormalType typeOfEntry nom t

def BibtexFile : Parsec (List Entry) := many' entry

--#eval BibtexQuery.name "auTHOr23:z  ".mkIterator
--#eval BibtexQuery.class "@ARTICLE ".mkIterator
--#eval BibtexQuery.tag "auTHOr =   \n{Dès Noël où un zéphyr haï\n me vêt de glaçons würmiens, je dîne d'exquis rôtis de bœuf au kir à l'aÿ d'âge mûr}".mkIterator
--#eval BibtexQuery.tag "auTHOr = \"Test\"".mkIterator
--#eval BibtexQuery.tag "journal = {Journal of Musical\n Deontology}".mkIterator
--#eval BibtexQuery.tag "year = 2022".mkIterator
--#eval BibtexQuery.entry "  @article{bla23,\n year = 2022,\n author = {Frédéric Dupuis}\n}\n".mkIterator
--#eval (Parsec.sepBy (manyChars $ Parsec.noneOf ",") (skipChar ',')) "bla, foo,".mkIterator

--#eval (sepBy' asciiWordToLower (do ws; skipChar ','; ws)) "bla, foo, baz, ".mkIterator


end BibtexQuery.Parser

open Lean
