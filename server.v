Require Extraction.

Inductive testAB: Type := TestA | TestB.

Extraction "server.ml" testAB.
