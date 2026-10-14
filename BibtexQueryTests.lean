/-
Tests for the Bibtex parser. Run with `lake build BibtexQueryTests`.
-/
import BibtexQuery.Parser
import BibtexQuery.Format

open BibtexQuery BibtexQuery.Parser BibtexQuery.ParsecExtra

namespace BibtexQuery.Tests

def parseFile (s : String) : Option (List Entry) := s.parse? bibtexFile

def fails (s : String) : Bool := (parseFile s).isNone

def article (key : String) (tags : List (String × String)) : Entry :=
  .normalType "article" key (tags.map fun (n, c) => { name := n, content := c })

-- The original tests.
#guard "auTHOr23:z  ".parse? (name) == some "auTHOr23:z"
#guard "@ARTICLE ".parse? category == some "article"
#guard "year = 2022".parse? tag == some { name := "year", content := "2022" }
#guard "Bdsk-Url-1 = {https://doi.org/10.1007/s00220-020-03839-5}".parse? tag
  == some { name := "bdsk-url-1", content := "https://doi.org/10.1007/s00220-020-03839-5" }
#guard "journal = {Journal of Musical\n Deontology}".parse? tag
  == some { name := "journal", content := "Journal of Musical\n Deontology" }
#guard "\"Bachem, Achim and Korte, Bernhard and Gr{\\\"o}tschel\"".parse? quotedContent
  == some "Bachem, Achim and Korte, Bernhard and Gr{\\\"o}tschel"
#guard parseFile "@article{bla23,\n year = 2022,\n author = {Frédéric Dupuis}\n}\n"
  == some [article "bla23" [("year", "2022"), ("author", "Frédéric Dupuis")]]

-- Cite keys: anything but whitespace, comma and the closing brace; case kept.
#guard parseFile "@article{Wło05, year = 2005}" == some [article "Wło05" [("year", "2005")]]
#guard parseFile "@article{2005paper, year = 2005}" == some [article "2005paper" [("year", "2005")]]
#guard parseFile "@article{kim.lee/2020+a:b, year = 2020}"
  == some [article "kim.lee/2020+a:b" [("year", "2020")]]
#guard parseFile "@article{ Key , year = 2020 }" == some [article "Key" [("year", "2020")]]
#guard fails "@article{, year = 2020}"
#guard fails "@article{a key, year = 2020}"

-- Parentheses as delimiters; a `)` in a key is fine, a `}` is not a delimiter there.
#guard parseFile "@article(k}ey, year = 2020)" == some [article "k}ey" [("year", "2020")]]
#guard fails "@article(key, year = 2020}"

-- Entries without fields, and a trailing comma.
#guard parseFile "@misc{key}" == some [.normalType "misc" "key" []]
#guard parseFile "@misc{key,}" == some [.normalType "misc" "key" []]
#guard parseFile "@misc{key, title = {T},}" == some [.normalType "misc" "key" [{ name := "title", content := "T" }]]

-- Entry types and field names are identifiers, lowercased.
#guard parseFile "@ Article{k, Title-Of.It = {T}}"
  == some [article "k" [("title-of.it", "T")]]
#guard fails "@article{k, 1title = {T}}"
#guard fails "@article{k, ti(tle = {T}}"

-- Field values: month macros (the only macros supported), and `#` concatenation.
#guard parseFile "@article{k, month = JAN}" == some [article "k" [("month", "jan")]]
#guard fails "@article{k, note = foo}"
#guard fails "@article{k, month = jan2}"
#guard parseFile "@article{k, title = \"A\" # { B} # 12 # jan}"
  == some [article "k" [("title", "A B12jan")]]

-- Quoted strings: a `\"` inside braces does not terminate, line breaks become spaces.
#guard parseFile "@article{k, author = \"Gr{\\\"o}tschel\"}"
  == some [article "k" [("author", "Gr{\\\"o}tschel")]]
#guard parseFile "@article{k, title = \"A\nB\"}" == some [article "k" [("title", "A B")]]
#guard fails "@article{k, title = \"A}B\"}"

-- Commands. `@comment` ignores nothing but itself; the rest is text outside of entries.
#guard parseFile "@comment{ @article{k, year = 2020} }"
  == some [.commentType, article "k" [("year", "2020")]]
#guard parseFile "@preamble{ \"\\newcommand{\\x}{y}\" }"
  == some [.preambleType "\\newcommand{\\x}{y}"]
#guard parseFile "@string(foo = {Bar})" == some [.stringType "foo = {Bar}"]

-- Text outside of entries is ignored; a malformed entry is an error, not a truncation.
#guard parseFile "junk @article{a, year = 2020} more junk\n@article{b, year = 2021}\n%trailing"
  == some [article "a" [("year", "2020")], article "b" [("year", "2021")]]
#guard parseFile "" == some []
#guard parseFile "no entries here" == some []
#guard fails "@article{a, year = 2020}\n@article{b, year = }"
#guard fails "@article{a, year = 2020}\n@"
#guard fails "@article{a, year = 2020"

-- Tags: generated from the authors and the year, with diacritics stripped as BibTeX's `alpha`
-- style does; or the `shorthand` field, as in biblatex.
def tagOf (e : Entry) : Option String := ((ProcessedEntry.ofEntry e).toOption.bind id).map (·.tag)

#guard tagOf (article "Kol07" [("author", "Kollár, János"), ("year", "2007")]) == some "[Kol07]"
#guard tagOf (article "Wło05" [("author", "Włodarczyk, Jarosław"), ("year", "2005")]) == some "[Wlo05]"
#guard tagOf (article "Wło05" [("author", "Włodarczyk, Jarosław"), ("year", "2005"),
    ("shorthand", "Wło05")]) == some "[Wło05]"
#guard tagOf (article "k" [("author", "Doe, John"), ("year", "2012"), ("shorthand", " ")])
  == some "[Doe12]"
#guard tagOf (article "BM88" [("author", "Bierstone, Edward and Milman, Pierre D."),
    ("year", "1988")]) == some "[BM88]"

end BibtexQuery.Tests
