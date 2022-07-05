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
| Key (s : String)
| Author (s : String)
| Title (s : String)
deriving Repr

def Query.ofString (s : String) : Option Query :=
if s.startsWith "k." then some $ Query.Key $ s.drop 2
else if s.startsWith "a." then some $ Query.Author $ s.drop 2
else if s.startsWith "t." then some $ Query.Title $ s.drop 2
else none

def Entry.matchQuery (e : Entry) (q : Query) : Bool := 
match q with
| Query.Key s     => e.getKey.flattenWords.containsSubstr s
| Query.Author s  => e.getAuthors.toFullNames.containsSubstr s
| Query.Title s   => e.getTitle.flattenWords.containsSubstr s

def Entry.matchQueries (e : Entry) (lq : List Query) : Bool :=
lq.foldl (fun acc q => acc && e.matchQuery q) true

end BibtexQuery