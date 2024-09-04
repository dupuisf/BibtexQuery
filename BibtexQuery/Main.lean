/-
Copyright (c) 2022 Frédéric Dupuis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Frédéric Dupuis
-/

-- Just a dummy comment -- please delete

import Lean.Data.HashMap
import BibtexQuery.Parser
import BibtexQuery.String
import BibtexQuery.Query
import BibtexQuery.Format

/-!
# BibtexQuery: a simple command-line bibtex query utility

BibtexQuery is a command-line utility that reads in a bibtex file and performs simple queries. A query is a string
of the form "t.querystring", where `t` is either `a` for author, `t` for title or `k` for key, and `querystring`
is a string (without spaces). BibtexQuery reads in a bibtex file, and returns the entries that match all the
queries given a command-line parameters. Note that the entries are processed in such a way that strips diacritics,
spaces and special characters before the queries are performed. In addition, the list of authors is normalized to
firstnamelastname. Hence, for example, "Dupuis, Frédéric" will match the query `a.ericdup`.
-/

open Lean BibtexQuery

def listDoublons (parseRes : List BibtexQuery.Entry) : List String :=
  let keysOnly := parseRes.filterMap (fun entry => match entry with
                                                   | BibtexQuery.Entry.normalType _ name _ => some name
                                                   | _ => none)
  let ⟨_, dupl⟩ : (Lean.HashMap String Unit) × List String :=
    keysOnly.foldl (init := ⟨Lean.HashMap.empty, []⟩)
      (fun ⟨hsh, lst⟩ key =>
        match hsh.find? key with
        | none => ⟨hsh.insert key (), lst⟩
        | some _ => ⟨hsh, (key :: lst)⟩)
  dupl

def printHelp := IO.println
"
bibtex-query - command-line bibtex file processor

Usage: bibtex-query command filename [args]

Commands:
  h: print this help message
  d: check for duplicate entries
  l: sort all entries and list them in plaintext form
  q: print entries that match the given query
  c: print citations of entries that match the given query

Queries have the form «t.query» (without the quotes) with t being the type of query,
and «query» being the content. The entries printed out are those that match all the queries.

Types of queries:
  k: key (ex: k.d14)
  c: class (ex: article, book)
  a: author (ex: a.dupuis)
  t: title (ex: t.channelcapacity)
  w: keywords (ex: w.quantum)
"

mutual

partial def eToPlaintext : Xml.Element → String
| .Element _ _ c => s!"{c.map cToPlaintext |>.foldl (· ++ ·) ""}"

partial def cToPlaintext : Xml.Content → String
| .Element e => eToPlaintext e
| .Comment _ => ""
| .Character c => c

end

def printEntry (e : ProcessedEntry) : IO Unit :=
  let s := e.html.map cToPlaintext |>.toList |> String.join
  IO.println (e.tag ++ "{" ++ e.name ++ "} " ++ s ++ "\n")

def printEntries (ents : List Entry) : IO Unit :=
  match ents.toArray.mapM ProcessedEntry.ofEntry with
  | .ok arr =>
    discard (arr.filterMap id |> sortEntry |> deduplicateTag |>.map ProcessedEntry.format
      |>.mapM printEntry)
  | .error err => throw <| IO.userError err

def printMatchingEntries (ents : List Entry) (qs : List Query) : IO Unit := do
  printEntries <| ents.filter (·.matchQueries qs)

def printMatchingCitations (ents : List Entry) (qs : List Query) : IO Unit := do
  for e in ents do
    if e.matchQueries qs then IO.println e.toCitation

def main : List String → IO Unit
  | ["h"]           => printHelp
  | ["-h"]          => printHelp
  | ["--help"]      => printHelp
  | ["h", _]        => printHelp
  | ["-h", _]       => printHelp
  | ["--help", _]   => printHelp
  | ["d", fname]    => do
    IO.println s!"Reading {fname} to find doubled keys"
    let parsed := BibtexQuery.Parser.bibtexFile (←IO.FS.readFile fname).iter
    match parsed with
    | .success _pos res =>
      let lst := listDoublons res
      IO.println lst
    | .error pos err => IO.eprintln s!"Parse error at line {pos.lineNumber}: {err}"
  | ["l", fname]    => do
    let parsed := BibtexQuery.Parser.bibtexFile (←IO.FS.readFile fname).iter
    match parsed with
    | .success _pos res => printEntries res
    | .error pos err => IO.eprint s!"Parse error at line {pos.lineNumber}: {err}"
  | "q" :: (fname :: queries) => do
    let parsed := BibtexQuery.Parser.bibtexFile (←IO.FS.readFile fname).iter
    match parsed with
    | .success _pos res => printMatchingEntries res $ queries.filterMap Query.ofString
    | .error pos err => IO.eprint s!"Parse error at line {pos.lineNumber}: {err}"
  | "c" :: (fname :: queries) => do
    let parsed := BibtexQuery.Parser.bibtexFile (←IO.FS.readFile fname).iter
    match parsed with
    | .success _pos res => printMatchingCitations res $ queries.filterMap Query.ofString
    | .error pos err => IO.eprint s!"Parse error at line {pos.lineNumber}: {err}"
  | _            => do IO.eprintln "Invalid command-line arguments"; printHelp
