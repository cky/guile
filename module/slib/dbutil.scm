;;; "dbutil.scm" relational-database-utilities
; Copyright 1994, 1995, 1997, 2000, 2001 Aubrey Jaffer
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(require 'relational-database)
(require 'common-list-functions)

(define (db:base-type path)
  'alist-table)				; currently the only one.

(define (dbutil:wrap-command-interface rdb)
  (and rdb
       (let* ((rdms:commands ((rdb 'open-table) '*commands* #f))
	      (command:get
	       (and rdms:commands (rdms:commands 'get 'procedure))))
	 (and command:get
	      (letrec ((wdb (lambda (command)
			      (let ((com (command:get command)))
				(cond (com ((slib:eval com) wdb))
				      (else (rdb command)))))))
		(let ((init (wdb '*initialize*)))
		  (if (procedure? init) init wdb)))))))

(define (dbutil:open-database! path . arg)
  (let ((type (if (null? arg) (db:base-type path) (car arg))))
    (require type)
    (dbutil:wrap-command-interface
     (((make-relational-system (slib:eval type)) 'open-database)
      path #t))))

(define (dbutil:open-database path . arg)
  (let ((type (if (null? arg) (db:base-type path) (car arg))))
    (require type)
    (dbutil:wrap-command-interface
     (((make-relational-system (slib:eval type)) 'open-database)
      path #f))))

(define (dbutil:check-domain rdb)
  (let* ((ro:domains ((rdb 'open-table) '*domains-data* #f))
	 (ro:get-dir (ro:domains 'get 'domain-integrity-rule))
	 (ro:for-tab (ro:domains 'get 'foreign-table)))
    (lambda (domain)
      (let ((fkname (ro:for-tab domain))
	    (dir (slib:eval (ro:get-dir domain))))
	(if fkname (let* ((fktab ((rdb 'open-table) fkname #f))
			  (p? (fktab 'get 1)))
		     (if dir (lambda (e) (and (dir e) (p? e))) p?))
	    dir)))))

(define (dbutil:create-database path type)
  (require type)
  (let ((rdb (((make-relational-system (slib:eval type)) 'create-database)
	      path)))
    (dbutil:define-tables
     rdb
     '(type
       ((name symbol))
       ()
       ((atom)
	(symbol)
	(string)
	(number)
	(money)
	(date-time)
	(boolean)
	(foreign-key)
	(expression)
	(virtual)))
     '(parameter-arity
       ((name symbol))
       ((predicate? expression)
	(procedure expression))
       ((single (lambda (a) (and (pair? a) (null? (cdr a)))) car)
	(optional
	 (lambda (lambda (a) (or (null? a) (and (pair? a) (null? (cdr a))))))
	 identity)
	(boolean
	 (lambda (a) (or (null? a)
			 (and (pair? a) (null? (cdr a)) (boolean? (car a)))))
	 (lambda (a) (if (null? a) #f (car a))))
	(nary (lambda (a) #t) identity)
	(nary1 (lambda (a) (not (null? a))) identity))))
    (for-each (((rdb 'open-table) '*domains-data* #t) 'row:insert)
	      '((parameter-list *catalog-data* #f symbol 1)
		(parameter-name-translation *catalog-data* #f symbol 1)
		(parameter-arity parameter-arity #f symbol 1)
		(table *catalog-data* #f atom 1)))
    (dbutil:define-tables
     rdb
     '(*parameter-columns*
       *columns*
       *columns*
       ((1 #t index #f uint)
	(2 #f name #f symbol)
	(3 #f arity #f parameter-arity)
	(4 #f domain #f domain)
	(5 #f defaulter #f expression)
	(6 #f expander #f expression)
	(7 #f documentation #f string)))
     '(no-parameters
       *parameter-columns*
       *parameter-columns*
       ())
     '(no-parameter-names
       ((name string))
       ((parameter-index uint))
       ())
     '(add-domain-params
       *parameter-columns*
       *parameter-columns*
       ((1 domain-name single atom #f #f "new domain name")
	(2 foreign-table optional table #f #f
	   "if present, domain-name must be existing key into this table")
	(3 domain-integrity-rule optional expression #f #f
	   "returns #t if single argument is good")
	(4 type-id single type #f #f "base type of new domain")
	(5 type-param optional expression #f #f
	   "which (key) field of the foreign-table")
	))
     '(add-domain-pnames
       ((name string))
       ((parameter-index uint))		;should be add-domain-params
       (
	("n" 1) ("name" 1)
	("f" 2) ("foreign (key) table" 2)
	("r" 3) ("domain integrity rule" 3)
	("t" 4) ("type" 4)
	("p" 5) ("type param" 5)
	))
     '(del-domain-params
       *parameter-columns*
       *parameter-columns*
       ((1 domain-name single domain #f #f "domain name")))
     '(del-domain-pnames
       ((name string))
       ((parameter-index uint))		;should be del-domain-params
       (("n" 1) ("name" 1)))
     '(*commands*
       ((name symbol))
       ((parameters parameter-list)
	(parameter-names parameter-name-translation)
	(procedure expression)
	(documentation string))
       ((domain-checker
	 no-parameters
	 no-parameter-names
	 dbutil:check-domain
	 "return procedure to check given domain name")

	(add-domain
	 add-domain-params
	 add-domain-pnames
	 (lambda (rdb)
	   (((rdb 'open-table) '*domains-data* #t) 'row:update))
	 "add a new domain")

	(delete-domain
	 del-domain-params
	 del-domain-pnames
	 (lambda (rdb)
	   (((rdb 'open-table) '*domains-data* #t) 'row:remove))
	 "delete a domain"))))
    (let* ((tab ((rdb 'open-table) '*domains-data* #t))
	   (row ((tab 'row:retrieve) 'type)))
      (set-car! (cdr row) 'type)
      ((tab 'row:update) row))
    (dbutil:wrap-command-interface rdb)))

(define (make-defaulter arity type)
  `(lambda (pl)
     ',(case arity
	 ((optional nary) '())
	 ((boolean) #f)
	 ((single nary1)
	  (case type
	    ((string) '(""))
	    ((symbol) '(nil))
	    (else '(#f))))
	 (else (slib:error 'make-defaulter 'unknown 'arity arity)))))

(define (get-foreign-choices tab)
  (define dlst ((tab 'get* 1)))
  (do ((dlst dlst (cdr dlst))
       (vlst (if (memq 'visible-name (tab 'column-names))
		 ((tab 'get* 'visible-name))
		 dlst)
	     (cdr vlst))
       (out '() (if (member (car dlst) (cdr dlst))
		    out
		    (cons (list (car dlst) (car vlst)) out))))
      ((null? dlst) out)))

(define (make-command-server rdb command-table)
  (let* ((comtab ((rdb 'open-table) command-table #f))
	 (names (comtab 'column-names))
	 (row-ref (lambda (row name) (list-ref row (position name names))))
	 (comgetrow (comtab 'row:retrieve)))
    (lambda (comname command-callback)
      (cond ((not comname) (set! comname '*default*)))
      (cond ((not (comgetrow comname))
	     (slib:error 'command 'not 'known: comname)))
      (let* ((command:row (comgetrow comname))
	     (parameter-table
	      ((rdb 'open-table) (row-ref command:row 'parameters) #f))
	     (parameter-names
	      ((rdb 'open-table) (row-ref command:row 'parameter-names) #f))
	     (comval ((slib:eval (row-ref command:row 'procedure)) rdb))
	     (options ((parameter-table 'get* 'name)))
	     (positions ((parameter-table 'get* 'index)))
	     (arities ((parameter-table 'get* 'arity)))
	     (defaulters (map slib:eval ((parameter-table 'get* 'defaulter))))
	     (domains ((parameter-table 'get* 'domain)))
	     (types (map (((rdb 'open-table) '*domains-data* #f) 'get 'type-id)
			 domains))
	     (dirs (map (rdb 'domain-checker) domains))
	     (aliases
	      (map list ((parameter-names 'get* 'name))
		   (map (parameter-table 'get 'name)
			((parameter-names 'get* 'parameter-index))))))
	(command-callback comname comval options positions
			  arities types defaulters dirs aliases)))))

(define (dbutil:define-tables rdb . spec-list)
  (define new-tables '())
  (define dom:typ (((rdb 'open-table) '*domains-data* #f) 'get 4))
  (define create-table (rdb 'create-table))
  (define open-table (rdb 'open-table))
  (define table-exists? (rdb 'table-exists?))
  (define (check-domain dname)
    (cond ((dom:typ dname))
	  ((member dname new-tables)
	   (let* ((ftab (open-table
			 (string->symbol
			  (string-append "desc:" (symbol->string dname)))
			 #f)))
	     ((((rdb 'open-table) '*domains-data* #t) 'row:insert)
	      (list dname dname #f
		    (dom:typ ((ftab 'get 'domain-name) 1)) 1))))))
  (define (define-table name prikeys slots data)
    (cond
     ((table-exists? name)
      (let* ((tab (open-table name #t))
	     (row:update (tab 'row:update)))
	(for-each row:update data)))
     ((and (symbol? prikeys) (eq? prikeys slots))
      (cond ((not (table-exists? slots))
	     (slib:error "Table doesn't exist:" slots)))
      (set! new-tables (cons name new-tables))
      (let* ((tab (create-table name slots))
	     (row:insert (tab 'row:insert)))
	(for-each row:insert data)
	((tab 'close-table))))
     (else
      (let* ((descname
	      (string->symbol (string-append "desc:" (symbol->string name))))
	     (tab (create-table descname))
	     (row:insert (tab 'row:insert))
	     (j 0))
	(set! new-tables (cons name new-tables))
	(for-each (lambda (des)
		    (set! j (+ 1 j))
		    (check-domain (cadr des))
		    (row:insert (list j #t (car des)
				      (if (null? (cddr des)) #f (caddr des))
				      (cadr des))))
		  prikeys)
	(for-each (lambda (des)
		    (set! j (+ 1 j))
		    (check-domain (cadr des))
		    (row:insert (list j #f (car des)
				      (if (null? (cddr des)) #f (caddr des))
				      (cadr des))))
		  slots)
	((tab 'close-table))
	(set! tab (create-table name descname))
	(set! row:insert (tab 'row:insert))
	(for-each row:insert data)
	((tab 'close-table))))))
  (for-each (lambda (spec) (apply define-table spec)) spec-list))

(define (dbutil:list-table-definition rdb table-name)
  (cond (((rdb 'table-exists?) table-name)
	 (let* ((table ((rdb 'open-table) table-name #f))
		(prilimit (table 'primary-limit))
		(coldefs (map list
			      (table 'column-names)
			      (table 'column-domains))))
	   (list table-name
		 (butnthcdr prilimit coldefs)
		 (nthcdr prilimit coldefs)
		 ((table 'row:retrieve*)))))
	(else #f)))

(define create-database dbutil:create-database)
(define open-database! dbutil:open-database!)
(define open-database dbutil:open-database)
(define define-tables dbutil:define-tables)
(define list-table-definition dbutil:list-table-definition)
