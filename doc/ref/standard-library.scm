;; The modules to document
(define *modules*
  '(((statprof)
     "Statistical profiler")
    ((sxml apply-templates)
     "A more XSLT-like approach to SXML transformations")
    ((sxml fold)
     "Fold-based SXML transformation operators")
    ((sxml simple)
     "Convenient XML parsing and serializing")
    ((sxml ssax)
     "Functional-style XML parsing for Scheme")
    ((sxml ssax input-parse)
     "The SSAX tokenizer, optimized for Guile")
    ((sxml transform)
     "A higher-order SXML transformation operator, "
     (code "pre-post-order"))
    ((sxml xpath)
     "XPath for SXML")
    ((texinfo)
     "Parse texinfo files or fragments into " (code "stexi") ", a "
     "scheme representation")
    ((texinfo docbook)
     "Transform a subset of docbook into " (code "stexi"))
    ((texinfo html)
     "Transform " (code "stexi") " into HTML")
    ((texinfo indexing)
     "Extract an index from a piece of " (code "stexi"))
    ((texinfo string-utils)
     "String utility functions used by the texinfo processor")
    ((texinfo plain-text)
     "Render " (code "stexi") " as plain text")
    ((texinfo serialize)
     "Render " (code "stexi") " as texinfo")
    ((texinfo reflection)
     "Enable texinfo across Guile's help system")))

(define *module-sources*
  '(((sxml ssax) . "http://ssax.sourceforge.net/")
    ((sxml xpath) . "http://ssax.sourceforge.net/")
    ((sxml transform) . "http://ssax.sourceforge.net/")
    ((sxml apply-templates) . "http://ssax.sourceforge.net/")
    ((sxml ssax input-parse) . "http://ssax.sourceforge.net/")
    ((htmlprag) . "http://neilvandyke.org/htmlprag/")))

(define *scripts* '())

;; arch-tag: e493ad42-ad58-451c-a2d6-b17ba6c1d1d0
