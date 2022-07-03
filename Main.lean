/-
Copyright (c) 2022 Frédéric Dupuis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Frédéric Dupuis
-/

import Std.Data.HashMap
import BibtexQuery.Parser
import BibtexQuery.String

open Lean

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

def printHelp := IO.println "FIXME This is a help message."

def main : List String → IO Unit
| ["h"]           => printHelp
| ["-h"]          => printHelp
| ["--help"]      => printHelp
| ["h", _]        => printHelp
| ["-h", _]       => printHelp
| ["--help", _]   => printHelp
| ["d", fname]    => do
  let file ← IO.FS.readFile fname
  IO.println s!"Reading {fname} to find doubled keys"
  let parsed := BibtexQuery.Parser.BibtexFile file.mkIterator
  match parsed with
  | Parsec.ParseResult.success pos res => 
    let lst := listDoublons res
    IO.println lst
  | Parsec.ParseResult.error pos err => IO.eprintln s!"Parse error at line {pos.lineNumber}: {err}"
| ["s", fname]    => do
  let file ← IO.FS.readFile fname
  let parsed := BibtexQuery.Parser.BibtexFile file.mkIterator
  match parsed with
  | Parsec.ParseResult.success pos res => IO.print $ reprStr res
  | Parsec.ParseResult.error pos err => IO.eprint s!"Parse error at line {pos.lineNumber}: {err}"
| _            => do IO.eprintln "Invalid command-line arguments"; printHelp

