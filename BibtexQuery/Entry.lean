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
  Name : String
  Content : String
deriving Repr

/-- A whole bibtex entry, i.e. a paper or a book. -/
inductive Entry where
  | NormalType (Class : String) (Name : String) (Tags : List Tag)
  | StringType (Content : String)
  | PreambleType (Content : String)
  | CommentType
deriving Repr

namespace Entry

/-- Returns the authors of a bibtex entry, or an empty string if it is not specified. -/
def getAuthors : (e : Entry) → String
| NormalType cl name tags =>
  match tags.find? (fun t => t.Name = "author") with
  | some t => t.Content
  | none   => ""
| _ => ""

/-- Returns the title of a bibtex entry, or an empty string if it is not specified. -/
def getTitle : (e : Entry) → String
| NormalType cl name tags =>
  match tags.find? (fun t => t.Name = "title") with
  | some t => t.Content
  | none   => ""
| _ => ""

/-- Returns the key of a bibtex entry. -/
def getKey : Entry → String
| NormalType cl name tags => name
| _ => "No key"

/-- Returns an abridged representation of a bibtex entry. -/
def toAbridgedRepr (e : Entry) : String := 
e.getKey.pad ' ' 25 ++ " | " ++ e.getAuthors.toLastNames.pad ' ' 50 ++ " | " ++ e.getTitle

end Entry

end BibtexQuery