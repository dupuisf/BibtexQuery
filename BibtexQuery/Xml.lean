/-
Copyright (c) 2021 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Dany Fabian

These types were previously part of Lean.Data.Xml.Basic in the Lean 4 core,
removed in https://github.com/leanprover/lean4/pull/12302.
Inlined here for BibtexQuery's use.
-/

import Std.Data.TreeMap.Basic

namespace BibtexQuery.Xml

def Attributes := Std.TreeMap String String compare

mutual

inductive Element where
  | Element
    (name : String)
    (attributes : Attributes)
    (content : Array Content)

inductive Content where
  | Element (element : Element)
  | Comment (comment : String)
  | Character (content : String)
  deriving Inhabited

end

mutual

partial def Element.toString : Element → String
  | .Element n a c =>
    let inner := c.map Content.toString |>.toList |> String.join
    let attrs := a.foldl (init := "") fun acc k v => acc ++ s!" {k}=\"{v}\""
    s!"<{n}{attrs}>{inner}</{n}>"

partial def Content.toString : Content → String
  | .Element e => e.toString
  | .Comment c => s!"<!--{c}-->"
  | .Character s => s

end

instance : ToString Element := ⟨Element.toString⟩
instance : ToString Content := ⟨Content.toString⟩

end BibtexQuery.Xml
