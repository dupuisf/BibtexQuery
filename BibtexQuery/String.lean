/-
Copyright (c) 2022 Frédéric Dupuis. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Frédéric Dupuis
-/

/-!
# String processing

This file contains various string processing functions.
-/

/-- Get the line number of the current position of the iterator. -/
def String.Iterator.lineNumber (it : String.Iterator) : Nat :=
  let s : Substring := ⟨it.toString, 0, it.pos⟩
  s.foldl (fun n c => if c = '\n' then n+1 else n) 1

def Char.asciify : Char → Char
| 'á' => 'a'
| 'à' => 'a'
| 'â' => 'a'
| 'ä' => 'a'
| 'æ' => 'a'
| 'ǎ' => 'a'
| 'ã' => 'a'
| 'å' => 'a'
| 'ą' => 'a'
| 'Á' => 'A'
| 'À' => 'A'
| 'Â' => 'A'
| 'Ä' => 'A'
| 'Æ' => 'A'
| 'Ǎ' => 'A'
| 'Ã' => 'A'
| 'Å' => 'A'
| 'Ą' => 'A'
| 'č' => 'c'
| 'ç' => 'c'
| 'ć' => 'c'
| 'Č' => 'C'
| 'Ç' => 'C'
| 'Ć' => 'C'
| 'ď' => 'd'
| 'Ď' => 'D'
| 'é' => 'e'
| 'è' => 'e'
| 'ê' => 'e'
| 'ë' => 'e'
| 'ę' => 'e'
| 'ě' => 'e'
| 'É' => 'E'
| 'È' => 'E'
| 'Ê' => 'E'
| 'Ë' => 'E'
| 'Ę' => 'E'
| 'ğ' => 'g'
| 'Ğ' => 'G'
| 'í' => 'i'
| 'ì' => 'i'
| 'î' => 'i'
| 'ï' => 'i'
| 'ı' => 'i'
| 'Í' => 'I'
| 'Ì' => 'I'
| 'Î' => 'I'
| 'Ï' => 'I'
| 'İ' => 'I'
| 'ł' => 'l'
| 'ľ' => 'l'
| 'Ł' => 'L'
| 'Ľ' => 'L'
| 'ñ' => 'n'
| 'ň' => 'n'
| 'ń' => 'n'
| 'Ñ' => 'N'
| 'Ň' => 'N'
| 'Ń' => 'N'
| 'ó' => 'o'
| 'ò' => 'o'
| 'ô' => 'o'
| 'ö' => 'o'
| 'õ' => 'o'
| 'ø' => 'o'
| 'œ' => 'o'
| 'Ó' => 'O'
| 'Ò' => 'O'
| 'Ô' => 'O'
| 'Ö' => 'O'
| 'Õ' => 'O'
| 'Ø' => 'O'
| 'Œ' => 'O'
| 'ř' => 'r'
| 'Ř' => 'R'
| 'š' => 's'
| 'ś' => 's'
| 'ş' => 's'
| 'Š' => 'S'
| 'Ś' => 'S'
| 'Ş' => 'S'
| 'ť' => 't'
| 'Ť' => 'T'
| 'ú' => 'u'
| 'ù' => 'u'
| 'û' => 'u'
| 'ü' => 'u'
| 'ů' => 'u'
| 'Ú' => 'U'
| 'Ù' => 'U'
| 'Û' => 'U'
| 'Ü' => 'U'
| 'Ů' => 'U'
| 'ý' => 'y'
| 'ÿ' => 'y'
| 'Ý' => 'Y'
| 'Ÿ' => 'Y'
| 'ž' => 'z'
| 'Ž' => 'Z'
| 'ß' => 's'
| c   => c

def String.asciify (s : String) : String := s.map Char.asciify

--#eval "Dès Noël où un zéphyr haï me vêt de glaçons würmiens, je dîne d'exquis rôtis de 
--bœuf au kir à l'aÿ d'âge mûr & cætera".asciify

--#eval "Testfile aisdfjoai".foldl (fun s c => s ++ "A") ""
--#eval '{'.asciify.toLower

def String.flattenWords (s : String) : String := s.foldl 
(fun s c => s ++ (if c.asciify.toLower.isAlphanum then c.asciify.toLower.toString else "")) ""

--#eval "Frédéric Dupuis, Marco {T}omamichel".flattenWords

def String.splitIntoNames (s : String) : List String :=
(s.splitOn (sep := " and ")).map trim

def String.toLastName (s : String) : String :=
let s' := (s.split (fun c => c = ',')).map trim
match s' with
| [s₁] => s₁
| (s₁ :: _) => s₁
| _ => ""

def String.toLastNames (s : String) : String :=
String.intercalate " " $ s.splitIntoNames.map String.toLastName

def String.flipName (s : String) : String :=
let s' := (s.split (fun c => c = ',')).map trim
match s' with
| [s₁] => s₁
| [s₁, s₂] => s₂ ++ " " ++ s₁
| _ => ""

def String.toFullNames (s : String) : String :=
String.join $ (s.splitIntoNames.map String.flipName).map String.flattenWords

partial def Substring.containsSubstrStartingAt (s : Substring) (q : String) : Bool :=
if s.toString.length = 0 then q.length = 0
else if q.isPrefixOf s.toString then true
else 
  (s.drop 1).containsSubstrStartingAt q
  
def String.containsSubstr (s : String) (q : String) : Bool :=
s.toSubstring.containsSubstrStartingAt q

def String.pad (s : String) (c : Char) (n : Nat) : String :=
(s ++ ⟨List.replicate n c⟩).take n
