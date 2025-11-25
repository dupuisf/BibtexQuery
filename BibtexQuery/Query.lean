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
| word (s : String)
| class (s : String)
deriving Repr, Inhabited

def Query.ofString (s : String) : Option Query :=
  if s.startsWith "k." then some <| .key <| s.drop 2 |>.copy
  else if s.startsWith "a." then some <| .author <| s.drop 2 |>.copy
  else if s.startsWith "t." then some <| .title <| s.drop 2 |>.copy
  else if s.startsWith "w." then some <| .word <| s.drop 2 |>.copy
  else if s.startsWith "c." then some <| .class <| s.drop 2 |>.copy
  else none

def Entry.matchQuery (e : Entry) (q : Query) : Bool :=
  match q with
  | .key s => e.getKey.flattenWords.containsSubstr s
  | .author s => e.getAuthors.toFullNames.containsSubstr s
  | .title s => e.getTitle.flattenWords.containsSubstr s
  | .word s => e.getKeywords.flattenWords.containsSubstr s
  | .class s => e.getClass.flattenWords.containsSubstr s

def Entry.matchQueries (e : Entry) (lq : List Query) : Bool :=
  lq.foldl (fun acc q => acc && e.matchQuery q) true

end BibtexQuery
