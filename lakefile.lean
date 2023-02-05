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

require std from git "https://github.com/leanprover/std4" @ "main"
