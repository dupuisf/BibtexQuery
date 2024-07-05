import Lake
open Lake DSL

package BibtexQuery

require «UnicodeBasic» from git
  "https://github.com/fgdorais/lean4-unicode-basic" @ "main"

@[default_target]
lean_lib BibtexQuery

@[default_target]
lean_exe «bibtex-query» where
    root := `BibtexQuery.Main
