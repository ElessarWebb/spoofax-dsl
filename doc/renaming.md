The Renamer
===========

The renamer is tasked with resolving all identifiers to their definitions.
It transforms the AST from AST[String] to AST[Term], replacing names with references.

In order to support forward references, the renamer does it's work in multiple phases:

1.  Forward declare everything that can be forward referenced
2.  Declare and resolve all other identifiers

During it's work, the renamer has to keep track of scopes and what is in it; it does so using a
hierarchical symtab.
Each "local" symbtab has a parent, with exception of the root scope.

Each symtab also keeps track of which definitions it scopes.
When a symbol is defined, it is defined in the first symtab that scopes it, going up from the
current scope.

Reading
-------

If you like that sort-a-thing:

+ http://www.cs.cornell.edu/courses/cs412/2008sp/lectures/lec12.pdf, Symboltable introduction
+ http://booksite.elsevier.com/9780123745149/appendices/data/chapters/3a_impsc.pdf, LeBlanc-Cook
  symbol table
