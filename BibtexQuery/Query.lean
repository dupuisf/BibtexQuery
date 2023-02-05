/-
Copyright (c) 2022 Frédéric Dupuis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Frédéric Dupuis
-/

import BibtexQuery.String
import BibtexQuery.Entry

/-!
# Bibtex Queries

This file deals with bibtex queries of the limited form handled by the program.
-/

namespace BibtexQuery

inductive Query where
| key (s : String)
| author (s : String)
| title (s : String)
deriving Repr

def Query.ofString (s : String) : Option Query :=
  if s.startsWith "k." then some $ Query.key $ s.drop 2
  else if s.startsWith "a." then some $ Query.author $ s.drop 2
  else if s.startsWith "t." then some $ Query.title $ s.drop 2
  else none

def Entry.matchQuery (e : Entry) (q : Query) : Bool := 
  match q with
  | Query.key s     => e.getKey.flattenWords.containsSubstr s
  | Query.author s  => e.getAuthors.toFullNames.containsSubstr s
  | Query.title s   => e.getTitle.flattenWords.containsSubstr s

def Entry.matchQueries (e : Entry) (lq : List Query) : Bool :=
  lq.foldl (fun acc q => acc && e.matchQuery q) true

end BibtexQuery