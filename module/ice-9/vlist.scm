;;; -*- mode: scheme; coding: utf-8; -*-
;;;
;;; Copyright (C) 2009, 2010, 2011 Free Software Foundation, Inc.
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 3 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

(define-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)

  #:export (vlist? vlist-cons vlist-head vlist-tail vlist-null?
            vlist-null list->vlist vlist-ref vlist-drop vlist-take
            vlist-length vlist-fold vlist-fold-right vlist-map
            vlist-unfold vlist-unfold-right vlist-append
            vlist-reverse vlist-filter vlist-delete vlist->list
            vlist-for-each
            block-growth-factor

            vhash? vhash-cons vhash-consq vhash-consv
            vhash-assoc vhash-assq vhash-assv
            vhash-delete vhash-delq vhash-delv
            vhash-fold
            vhash-fold* vhash-foldq* vhash-foldv*
            alist->vhash))

;;; Author: Ludovic Courtès <ludo@gnu.org>
;;;
;;; Commentary:
;;;
;;; This module provides an implementations of vlists, a functional list-like
;;; data structure described by Phil Bagwell in "Fast Functional Lists,
;;; Hash-Lists, Dequeues and Variable-Length Arrays", EPFL Technical Report,
;;; 2002.
;;;
;;; The idea is to store vlist elements in increasingly large contiguous blocks
;;; (implemented as vectors here).  These blocks are linked to one another using
;;; a pointer to the next block (called `block-base' here) and an offset within
;;; that block (`block-offset' here).  The size of these blocks form a geometric
;;; series with ratio `block-growth-factor'.
;;;
;;; In the best case (e.g., using a vlist returned by `list->vlist'),
;;; elements from the first half of an N-element vlist are accessed in O(1)
;;; (assuming `block-growth-factor' is 2), and `vlist-length' takes only
;;; O(ln(N)).  Furthermore, the data structure improves data locality since
;;; vlist elements are adjacent, which plays well with caches.
;;;
;;; Code:


;;;
;;; VList Blocks and Block Descriptors.
;;;

(define block-growth-factor
  (let ((f (make-fluid)))
    (fluid-set! f 2)
    f))

(define-syntax define-inline
  ;; Work around the lack of an inliner.
  (syntax-rules ()
    ((_ (name formals ...) body ...)
     (define-syntax name
       (syntax-rules ()
         ((_ formals ...)
          (begin body ...)))))))

(define-inline (make-block base offset size hash-tab?)
  ;; Return a block (and block descriptor) of SIZE elements pointing to BASE
  ;; at OFFSET.  If HASH-TAB? is true, a "hash table" is also added.
  ;; Note: We use `next-free' instead of `last-used' as suggested by Bagwell.

  ;; XXX: We could improve locality here by having a single vector but currently
  ;; the extra arithmetic outweighs the benefits (!).
  (vector (make-vector size)
          base offset size 0
          (and hash-tab? (make-vector size #f))))

(define-syntax define-block-accessor
  (syntax-rules ()
    ((_ name index)
     (define-inline (name block)
       (vector-ref block index)))))

(define-block-accessor block-content 0)
(define-block-accessor block-base 1)
(define-block-accessor block-offset 2)
(define-block-accessor block-size 3)
(define-block-accessor block-next-free 4)
(define-block-accessor block-hash-table 5)

(define-inline (increment-block-next-free! block)
  (vector-set! block 4
               (+ (block-next-free block) 1)))

(define-inline (block-append! block value)
  ;; This is not thread-safe.  To fix it, see Section 2.8 of the paper.
  (let ((offset (block-next-free block)))
    (increment-block-next-free! block)
    (vector-set! (block-content block) offset value)
    #t))

(define-inline (block-ref block offset)
  (vector-ref (block-content block) offset))

(define-inline (block-ref* block offset)
  (let ((v (block-ref block offset)))
    (if (block-hash-table block)
        (car v) ;; hide the vhash link
        v)))

(define-inline (block-hash-table-ref block offset)
  (vector-ref (block-hash-table block) offset))

(define-inline (block-hash-table-set! block offset value)
  (vector-set! (block-hash-table block) offset value))

(define block-null
  ;; The null block.
  (make-block #f 0 0 #f))


;;;
;;; VLists.
;;;

(define-record-type <vlist>
  ;; A vlist is just a base+offset pair pointing to a block.

  ;; XXX: Allocating a <vlist> record in addition to the block at each
  ;; `vlist-cons' call is inefficient.  However, Bagwell's hack to avoid it
  ;; (Section 2.2) would require GC_ALL_INTERIOR_POINTERS, which would be a
  ;; performance hit for everyone.
  (make-vlist base offset)
  vlist?
  (base    vlist-base)
  (offset  vlist-offset))

(set-record-type-printer! <vlist>
                          (lambda (vl port)
                            (cond ((vlist-null? vl)
                                   (format port "#<vlist ()>"))
                                  ((block-hash-table (vlist-base vl))
                                   (format port "#<vhash ~x ~a pairs>"
                                           (object-address vl)
                                           (vhash-fold (lambda (k v r)
                                                         (+ 1 r))
                                                       0
                                                       vl)))
                                  (else
                                   (format port "#<vlist ~a>"
                                           (vlist->list vl))))))


(define vlist-null
  ;; The empty vlist.
  (make-vlist block-null 0))

(define-inline (block-cons item vlist hash-tab?)
  (let loop ((base   (vlist-base vlist))
             (offset (+ 1 (vlist-offset vlist))))
    (if (and (< offset (block-size base))
             (= offset (block-next-free base))
             (block-append! base item))
        (make-vlist base offset)
        (let ((size (cond ((eq? base block-null) 1)
                          ((< offset (block-size base))
                           ;; new vlist head
                           1)
                          (else
                           (* (fluid-ref block-growth-factor)
                              (block-size base))))))
          ;; Prepend a new block pointing to BASE.
          (loop (make-block base (- offset 1) size hash-tab?)
                0)))))

(define (vlist-cons item vlist)
  "Return a new vlist with @var{item} as its head and @var{vlist} as its
tail."
  ;; Note: Calling `vlist-cons' on a vhash will not do the right thing: it
  ;; doesn't box ITEM so that it can have the hidden "next" link used by
  ;; vhash items, and it passes `#f' as the HASH-TAB? argument to
  ;; `block-cons'.  However, inserting all the checks here has an important
  ;; performance penalty, hence this choice.
  (block-cons item vlist #f))

(define (vlist-head vlist)
  "Return the head of @var{vlist}."
  (let ((base   (vlist-base vlist))
        (offset (vlist-offset vlist)))
    (block-ref* base offset)))

(define (vlist-tail vlist)
  "Return the tail of @var{vlist}."
  (let ((base   (vlist-base vlist))
        (offset (vlist-offset vlist)))
    (if (> offset 0)
        (make-vlist base (- offset 1))
        (make-vlist (block-base base)
                    (block-offset base)))))

(define (vlist-null? vlist)
  "Return true if @var{vlist} is empty."
  (let ((base (vlist-base vlist)))
    (and (not (block-base base))
         (= 0 (block-size base)))))


;;;
;;; VList Utilities.
;;;

(define (list->vlist lst)
  "Return a new vlist whose contents correspond to @var{lst}."
  (vlist-reverse (fold vlist-cons vlist-null lst)))

(define (vlist-fold proc init vlist)
  "Fold over @var{vlist}, calling @var{proc} for each element."
  ;; FIXME: Handle multiple lists.
  (let loop ((base   (vlist-base vlist))
             (offset (vlist-offset vlist))
             (result init))
    (if (eq? base block-null)
        result
        (let* ((next  (- offset 1))
               (done? (< next 0)))
          (loop (if done? (block-base base) base)
                (if done? (block-offset base) next)
                (proc (block-ref* base offset) result))))))

(define (vlist-fold-right proc init vlist)
  "Fold over @var{vlist}, calling @var{proc} for each element, starting from
the last element."
  (vlist-fold proc init (vlist-reverse vlist)))

(define (vlist-reverse vlist)
  "Return a new @var{vlist} whose content are those of @var{vlist} in reverse
order."
  (vlist-fold vlist-cons vlist-null vlist))

(define (vlist-map proc vlist)
  "Map @var{proc} over the elements of @var{vlist} and return a new vlist."
  (vlist-fold (lambda (item result)
                (vlist-cons (proc item) result))
              vlist-null
              (vlist-reverse vlist)))

(define (vlist->list vlist)
  "Return a new list whose contents match those of @var{vlist}."
  (vlist-fold-right cons '() vlist))

(define (vlist-ref vlist index)
  "Return the element at index @var{index} in @var{vlist}."
  (let loop ((index   index)
             (base    (vlist-base vlist))
             (offset  (vlist-offset vlist)))
    (if (<= index offset)
        (block-ref* base (- offset index))
        (loop (- index offset 1)
              (block-base base)
              (block-offset base)))))

(define (vlist-drop vlist count)
  "Return a new vlist that does not contain the @var{count} first elements of
@var{vlist}."
  (let loop ((count  count)
             (base   (vlist-base vlist))
             (offset (vlist-offset vlist)))
    (if (<= count offset)
        (make-vlist base (- offset count))
        (loop (- count offset 1)
              (block-base base)
              (block-offset base)))))

(define (vlist-take vlist count)
  "Return a new vlist that contains only the @var{count} first elements of
@var{vlist}."
  (let loop ((count  count)
             (vlist  vlist)
             (result vlist-null))
    (if (= 0 count)
        (vlist-reverse result)
        (loop (- count 1)
              (vlist-tail vlist)
              (vlist-cons (vlist-head vlist) result)))))

(define (vlist-filter pred vlist)
  "Return a new vlist containing all the elements from @var{vlist} that
satisfy @var{pred}."
  (vlist-fold-right (lambda (e v)
                      (if (pred e)
                          (vlist-cons e v)
                          v))
                    vlist-null
                    vlist))

(define* (vlist-delete x vlist #:optional (equal? equal?))
  "Return a new vlist corresponding to @var{vlist} without the elements
@var{equal?} to @var{x}."
  (vlist-filter (lambda (e)
                  (not (equal? e x)))
                vlist))

(define (vlist-length vlist)
  "Return the length of @var{vlist}."
  (let loop ((base (vlist-base vlist))
             (len  (vlist-offset vlist)))
    (if (eq? base block-null)
        len
        (loop (block-base base)
              (+ len 1 (block-offset base))))))

(define* (vlist-unfold p f g seed
                       #:optional (tail-gen (lambda (x) vlist-null)))
  "Return a new vlist.  See the description of SRFI-1 `unfold' for details."
  (let uf ((seed seed))
    (if (p seed)
        (tail-gen seed)
        (vlist-cons (f seed)
                    (uf (g seed))))))

(define* (vlist-unfold-right p f g seed #:optional (tail vlist-null))
  "Return a new vlist.  See the description of SRFI-1 `unfold-right' for
details."
  (let uf ((seed seed) (lis tail))
    (if (p seed)
        lis
        (uf (g seed) (vlist-cons (f seed) lis)))))

(define (vlist-append . vlists)
  "Append the given lists."
  (if (null? vlists)
      vlist-null
      (fold-right (lambda (vlist result)
                    (vlist-fold-right (lambda (e v)
                                        (vlist-cons e v))
                                      result
                                      vlist))
                  vlist-null
                  vlists)))

(define (vlist-for-each proc vlist)
  "Call @var{proc} on each element of @var{vlist}.  The result is unspecified."
  (vlist-fold (lambda (item x)
                (proc item))
              (if #f #f)
              vlist))


;;;
;;; Hash Lists, aka. `VHash'.
;;;

;; Assume keys K1 and K2, H = hash(K1) = hash(K2), and two values V1 and V2
;; associated with K1 and K2, respectively.  The resulting layout is a
;; follows:
;;
;;     ,--------------------.
;;     | ,-> (K1 . V1) ---. |
;;     | |                | |
;;     | |   (K2 . V2) <--' |
;;     | |                  |
;;     +-|------------------+
;;     | |                  |
;;     | |                  |
;;     | `-- O <---------------H
;;     |                    |
;;     `--------------------'
;;
;; The bottom part is the "hash table" part of the vhash, as returned by
;; `block-hash-table'; the other half is the data part.  O is the offset of
;; the first value associated with a key that hashes to H in the data part.
;; The (K1 . V1) pair has a "hidden" link to the (K2 . V2) pair; hiding the
;; link is handled by `block-ref'.

;; This API potentially requires users to repeat which hash function and which
;; equality predicate to use.  This can lead to unpredictable results if they
;; are used in consistenly, e.g., between `vhash-cons' and `vhash-assoc', which
;; is undesirable, as argued in http://savannah.gnu.org/bugs/?22159 .  OTOH, two
;; arguments can be made in favor of this API:
;;
;;  - It's consistent with how alists are handled in SRFI-1.
;;
;;  - In practice, users will probably consistenly use either the `q', the `v',
;;    or the plain variant (`vlist-cons' and `vlist-assoc' without any optional
;;    argument), i.e., they will rarely explicitly pass a hash function or
;;    equality predicate.

(define (vhash? obj)
  "Return true if @var{obj} is a hash list."
  (and (vlist? obj)
       (let ((base (vlist-base obj)))
         (and base
              (vector? (block-hash-table base))))))

(define* (vhash-cons key value vhash #:optional (hash hash))
  "Return a new hash list based on @var{vhash} where @var{key} is associated
with @var{value}.  Use @var{hash} to compute @var{key}'s hash."
  (let* ((key+value (cons key value))
         (entry     (cons key+value #f))
         (vlist     (block-cons entry vhash #t))
         (base      (vlist-base vlist))
         (khash     (hash key (block-size base))))

    (let ((o (block-hash-table-ref base khash)))
      (if o (set-cdr! entry o)))

    (block-hash-table-set! base khash
                           (vlist-offset vlist))

    vlist))

(define vhash-consq (cut vhash-cons <> <> <> hashq))
(define vhash-consv (cut vhash-cons <> <> <> hashv))

(define-inline (%vhash-fold* proc init key vhash equal? hash)
  ;; Fold over all the values associated with KEY in VHASH.
  (define khash
    (let ((size (block-size (vlist-base vhash))))
      (and (> size 0) (hash key size))))

  (let loop ((base       (vlist-base vhash))
             (khash      khash)
             (offset     (and khash
                              (block-hash-table-ref (vlist-base vhash)
                                                    khash)))
             (max-offset (vlist-offset vhash))
             (result     init))

    (let ((answer (and offset (block-ref base offset))))
      (cond ((and (pair? answer)
                  (<= offset max-offset)
                  (let ((answer-key (caar answer)))
                    (equal? key answer-key)))
             (let ((result      (proc (cdar answer) result))
                   (next-offset (cdr answer)))
               (loop base khash next-offset max-offset result)))
            ((and (pair? answer) (cdr answer))
             =>
             (lambda (next-offset)
               (loop base khash next-offset max-offset result)))
            (else
             (let ((next-base (block-base base)))
               (if (and next-base (> (block-size next-base) 0))
                   (let* ((khash  (hash key (block-size next-base)))
                          (offset (block-hash-table-ref next-base khash)))
                     (loop next-base khash offset (block-offset base)
                           result))
                   result)))))))

(define* (vhash-fold* proc init key vhash
                      #:optional (equal? equal?) (hash hash))
  "Fold over all the values associated with @var{key} in @var{vhash}, with each
call to @var{proc} having the form @code{(proc value result)}, where
@var{result} is the result of the previous call to @var{proc} and @var{init} the
value of @var{result} for the first call to @var{proc}."
  (%vhash-fold* proc init key vhash equal? hash))

(define (vhash-foldq* proc init key vhash)
  "Same as @code{vhash-fold*}, but using @code{hashq} and @code{eq?}."
  (%vhash-fold* proc init key vhash eq? hashq))

(define (vhash-foldv* proc init key vhash)
  "Same as @code{vhash-fold*}, but using @code{hashv} and @code{eqv?}."
  (%vhash-fold* proc init key vhash eqv? hashv))

(define-inline (%vhash-assoc key vhash equal? hash)
  ;; A specialization of `vhash-fold*' that stops when the first value
  ;; associated with KEY is found or when the end-of-list is reached.  Inline to
  ;; make sure `vhash-assq' gets to use the `eq?' instruction instead of calling
  ;; the `eq?'  subr.
  (define khash
    (let ((size (block-size (vlist-base vhash))))
      (and (> size 0) (hash key size))))

  (let loop ((base       (vlist-base vhash))
             (khash      khash)
             (offset     (and khash
                              (block-hash-table-ref (vlist-base vhash)
                                                    khash)))
             (max-offset (vlist-offset vhash)))
    (let ((answer (and offset (block-ref base offset))))
      (cond ((and (pair? answer)
                  (<= offset max-offset)
                  (let ((answer-key (caar answer)))
                    (equal? key answer-key)))
             (car answer))
            ((and (pair? answer) (cdr answer))
             =>
             (lambda (next-offset)
               (loop base khash next-offset max-offset)))
            (else
             (let ((next-base (block-base base)))
               (and next-base
                    (> (block-size next-base) 0)
                    (let* ((khash  (hash key (block-size next-base)))
                           (offset (block-hash-table-ref next-base khash)))
                      (loop next-base khash offset
                            (block-offset base))))))))))

(define* (vhash-assoc key vhash #:optional (equal? equal?) (hash hash))
  "Return the first key/value pair from @var{vhash} whose key is equal to
@var{key} according to the @var{equal?} equality predicate."
  (%vhash-assoc key vhash equal? hash))

(define (vhash-assq key vhash)
  "Return the first key/value pair from @var{vhash} whose key is @code{eq?} to
@var{key}."
  (%vhash-assoc key vhash eq? hashq))

(define (vhash-assv key vhash)
  "Return the first key/value pair from @var{vhash} whose key is @code{eqv?} to
@var{key}."
  (%vhash-assoc key vhash eqv? hashv))

(define* (vhash-delete key vhash #:optional (equal? equal?) (hash hash))
  "Remove all associations from @var{vhash} with @var{key}, comparing keys
with @var{equal?}."
  (if (vhash-assoc key vhash equal? hash)
      (vlist-fold (lambda (k+v result)
                    (let ((k (car k+v))
                          (v (cdr k+v)))
                      (if (equal? k key)
                          result
                          (vhash-cons k v result hash))))
                  vlist-null
                  vhash)
      vhash))

(define vhash-delq (cut vhash-delete <> <> eq? hashq))
(define vhash-delv (cut vhash-delete <> <> eqv? hashv))

(define (vhash-fold proc seed vhash)
  "Fold over the key/pair elements of @var{vhash}.  For each pair call
@var{proc} as @code{(@var{proc} key value result)}."
  (vlist-fold (lambda (key+value result)
                (proc (car key+value) (cdr key+value)
                      result))
              seed
              vhash))

(define* (alist->vhash alist #:optional (hash hash))
  "Return the vhash corresponding to @var{alist}, an association list."
  (fold-right (lambda (pair result)
                (vhash-cons (car pair) (cdr pair) result hash))
              vlist-null
              alist))

;;; vlist.scm ends here
