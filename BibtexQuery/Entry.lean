/-
Copyright (c) 2022 Frédéric Dupuis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Frédéric Dupuis
-/

import BibtexQuery.String

/-!
# Bibtex Entries and related functions

This file defines inductive types for bibtex entries and tags, along with various related
helper functions.
-/

namespace BibtexQuery

/-- i.e. authors = "Binne, Chose and Truc, Machin" -/
structure Tag where
  name : String
  content : String
deriving Repr

namespace Tag

/-- Return a standardized representation of the tag. -/
def toString (t : Tag) : String := s!"{t.name} = " ++ "{" ++ s!"{t.content}" ++ "}"

--#eval Tag.toString $ Tag.mk "author" "Frédéric Dupuis"

end Tag

/-- A whole bibtex entry, i.e. a paper or a book. -/
inductive Entry where
  | normalType (category : String) (name : String) (tags : List Tag)
  | stringType (content : String)
  | preambleType (content : String)
  | commentType
deriving Repr

namespace Entry

/-- Returns the authors of a bibtex entry, or an empty string if it is not specified. -/
def getAuthors : (e : Entry) → String
  | .normalType _cl _name tags =>
    match tags.find? (fun t => t.name = "author") with
    | some t => t.content
    | none   => ""
  | _ => ""

/-- Returns the title of a bibtex entry, or an empty string if it is not specified. -/
def getTitle : (e : Entry) → String
  | .normalType _cl _name tags =>
    match tags.find? (fun t => t.name = "title") with
    | some t => t.content
    | none   => ""
  | _ => ""

/-- Returns the keywords of a bibtex entry, or an empty string if it is not specified. -/
def getKeywords : (e : Entry) → String
  | normalType _cl _name tags =>
    match tags.find? (fun t => t.name = "keywords") with
    | some t => t.content
    | none   => ""
  | _ => ""

/-- Returns the key of a bibtex entry. -/
def getKey : Entry → String
  | .normalType _cl name _tags => name
  | _ => "No key"

/-- Returns the arxiv number of a bibtex entry. -/
def getArxivNumber : Entry → String
  | .normalType _cl _name tags => 
    match tags.find? (fun t => t.name = "eprint") with
    | some t => t.content
    | none   => ""
  | _ => "No arxiv number"

/-- Returns the arxiv link of a bibtex entry. -/
def getArxivLink : Entry → String
  | .normalType _cl _name tags => 
    match tags.find? (fun t => t.name = "eprint") with
    | some t => "http://arxiv.org/abs/" ++ t.content
    | none   => ""
  | _ => "No arxiv number"

/-- Returns an abridged representation of a bibtex entry. -/
def toAbridgedRepr (e : Entry) : String := 
  e.getKey.pad ' ' 25 ++ " | " ++ e.getAuthors.toLastNames.pad ' ' 50 ++ " | " ++ e.getTitle

/-- Returns a «clean citation» of a bibtex entry. -/
def toCitation (e : Entry) : String :=
  e.getAuthors ++ ", «" ++ e.getTitle ++ "», " ++ e.getArxivLink
  

/-- Returns a string containing a standardized representation of a bibtex entry. -/
def toString : Entry → String
  | .normalType cl name tags => Id.run do
      let mut output := (s!"@{cl}" ++ "{" ++ s!"{name},").push '\n'
      for t in tags do
        output := output ++ "  " ++ t.toString ++ s!"\n"
      return (output ++ "}")
  | _ => ""

--#eval IO.print <| Entry.toString $ .normalType "book" "d12" [Tag.mk "author" "Dupuis, Frédéric", 
--                                                 Tag.mk "title" "Bonsoir la visite"]

--#eval IO.print <| Entry.toCitation $ .normalType "book" "d12" [Tag.mk "author" "Dupuis, Frédéric", 
--                                                 Tag.mk "title" "Bonsoir la visite", Tag.mk "eprint" "2308.11736"]

end Entry

end BibtexQuery