/-
Copyright (c) 2022 Frédéric Dupuis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Frédéric Dupuis
-/

import Std.Data.HashMap
import BibtexQuery.Parser
import BibtexQuery.String
import BibtexQuery.Query

open Lean BibtexQuery

def listDoublons (parseRes : List BibtexQuery.Entry) : List String :=
  let keysOnly := parseRes.filterMap (fun entry => match entry with
                                                   | BibtexQuery.Entry.NormalType _ name _ => some name
                                                   | _ => none)
  let ⟨hash, dupl⟩ : (Std.HashMap String Unit) × List String :=
    keysOnly.foldl (init := ⟨Std.HashMap.empty, []⟩)
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
  l: list all entries in abridged form
  q: print entries that match the given query

Queries have the form «t.query» (without the quotes) with t being the type of query,
and «query» being the content. The entries printed out are those that match all the queries.

Types of queries:
  k: key (ex: k.d14)
  a: author (ex: a.dupuis)
  t: title (ex: t.channelcapacity)
"

def printEntries (ents : List Entry) : IO Unit :=
  for e in ents do
    IO.println e.toAbridgedRepr

def printMatchingEntries (ents : List Entry) (qs : List Query) : IO Unit := do
  for e in ents do
    if e.matchQueries qs then IO.println e.toAbridgedRepr 

def main : List String → IO Unit
| ["h"]           => printHelp
| ["-h"]          => printHelp
| ["--help"]      => printHelp
| ["h", _]        => printHelp
| ["-h", _]       => printHelp
| ["--help", _]   => printHelp
| ["d", fname]    => do
  IO.println s!"Reading {fname} to find doubled keys"
  let parsed := BibtexQuery.Parser.BibtexFile (←IO.FS.readFile fname).iter
  match parsed with
  | Parsec.ParseResult.success pos res => 
    let lst := listDoublons res
    IO.println lst
  | Parsec.ParseResult.error pos err => IO.eprintln s!"Parse error at line {pos.lineNumber}: {err}"
| ["l", fname]    => do
  let parsed := BibtexQuery.Parser.BibtexFile (←IO.FS.readFile fname).iter
  match parsed with
  | Parsec.ParseResult.success pos res => printEntries res
  | Parsec.ParseResult.error pos err => IO.eprint s!"Parse error at line {pos.lineNumber}: {err}"
| "q" :: (fname :: queries) => do 
  let parsed := BibtexQuery.Parser.BibtexFile (←IO.FS.readFile fname).iter
  match parsed with
  | Parsec.ParseResult.success pos res => printMatchingEntries res $ queries.filterMap Query.ofString
  --| Parsec.ParseResult.success pos res => testPrintMatchingEntries res $ queries.filterMap Query.ofString
  | Parsec.ParseResult.error pos err => IO.eprint s!"Parse error at line {pos.lineNumber}: {err}"
| _            => do IO.eprintln "Invalid command-line arguments"; printHelp

