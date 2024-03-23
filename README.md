# BibtexQuery: a simple command-line bibtex query utility

BibtexQuery is a command-line utility that reads in a bibtex file and performs simple queries. A query is a string
of the form ``q.querystring``, where ``q`` is either ``a`` for author, ``t`` for title, ``k`` for key, ``c`` for class (i.e. book, article, etc), or ``w`` for keywords, and ``querystring``
is a string (without spaces). BibtexQuery reads in a bibtex file, and returns the entries that match all the
queries given as command-line parameters. Note that the entries are processed in such a way that strips diacritics,
spaces and special characters before the queries are performed. In addition, the list of authors is normalized to
``firstnamelastname``. Hence, for example, ``Dupuis, Frédéric`` will match the query ``a.ericdup``.

Note that currently, only a subset of the official Bibtex format is supported; features such as predefined strings and concatenation using ``#`` are not supported. It is unclear whether this
will ever be supported in the future; I only wrote this to learn how to do "normal programming" in [Lean 4](https://github.com/leanprover/lean4/), and I don't personally use these Bibtex features.

## Installation instructions

Make sure you have a working Lean 4 installation (see [here](https://leanprover.github.io/lean4/doc/quickstart.html) for how to do that), and then type `lake build` in the main directory. The
executable should appear in ``.lake/build/bin``.
