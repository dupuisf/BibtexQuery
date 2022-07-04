/-
Copyright (c) 2022 Frédéric Dupuis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Frédéric Dupuis
-/

import BibtexQuery.String

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

def getAuthors : (e : Entry) → String
| NormalType cl name tags =>
  match tags.find? (fun t => t.Name = "author") with
  | some t => t.Content
  | none   => ""
| _ => ""

def getTitle : (e : Entry) → String
| NormalType cl name tags =>
  match tags.find? (fun t => t.Name = "title") with
  | some t => t.Content
  | none   => ""
| _ => ""

def getKey : Entry → String
| NormalType cl name tags => name
| _ => "No key"

def toAbridgedRepr (e : Entry) : String := 
e.getKey ++ " | " ++ e.getAuthors.toLastNames ++ " | " ++ e.getTitle

end Entry

end BibtexQuery