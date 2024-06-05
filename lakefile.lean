import Lake
open Lake DSL

package «bibtex-query» {
  -- add configuration options here
}

lean_lib «BibtexQuery» 

@[default_target]
lean_exe «bibtex-query» {
    root := `Main
}

require batteries from git "https://github.com/leanprover-community/batteries" @ "stable"
