;;;; 	Copyright (C) 2010 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2.1 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;;


(define-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (void
            float double
            int unsigned-int long unsigned-long size_t
            int8 uint8
            uint16 int16
            uint32 int32
            uint64 int64

            sizeof alignof

            %null-pointer
            null-pointer?
            make-pointer
            pointer-address

            pointer->bytevector
            bytevector->pointer
            set-pointer-finalizer!

            dereference-pointer
            string->pointer
            pointer->string

            pointer->procedure
            ;; procedure->pointer (see below)
            make-c-struct parse-c-struct))

(eval-when (load eval compile)
  (load-extension (string-append "libguile-" (effective-version))
                  "scm_init_foreign"))


;;;
;;; Pointers.
;;;

(define (null-pointer? pointer)
  "Return true if POINTER is the null pointer."
  (= (pointer-address pointer) 0))

(if (defined? 'procedure->pointer)
    (export procedure->pointer))


;;;
;;; Structures.
;;;

(define-syntax compile-time-value
  (syntax-rules ()
    "Evaluate the given expression at compile time.  The expression must
evaluate to a simple datum."
    ((_ exp)
     (let-syntax ((v (lambda (s)
                       (let ((val exp))
                         (syntax-case s ()
                           (_ (datum->syntax s val)))))))
       v))))

(eval-when (eval compile load)
  ;; The procedures below are used at compile time by the macros below.

  (define (integer-ref type signed?)
    (case (sizeof type)
      ((8) (if signed?
               'bytevector-s64-native-ref
               'bytevector-u64-native-ref))
      ((4) (if signed?
               'bytevector-s32-native-ref
               'bytevector-u32-native-ref))
      (else
       (error "what machine is this?" type (sizeof type)))))

  (define (integer-set type signed?)
    (case (sizeof type)
      ((8) (if signed?
               'bytevector-s64-native-set!
               'bytevector-u64-native-set!))
      ((4) (if signed?
               'bytevector-s32-native-set!
               'bytevector-u32-native-set!))
      (else
       (error "what machine is this?" type (sizeof type))))))

(define-syntax define-integer-reader
  (syntax-rules ()
    ((_ name type signed?)
     (letrec-syntax ((ref (identifier-syntax
                           (compile-time-value
                            (integer-ref type signed?)))))
       (define name ref)))))

(define-syntax define-integer-writer
  (syntax-rules ()
    ((_ name type signed?)
     (letrec-syntax ((set (identifier-syntax
                           (compile-time-value
                            (integer-set type signed?)))))
       (define name set)))))


(define-integer-reader %read-int int #t)
(define-integer-reader %read-long long #t)
(define-integer-writer %write-int! int #t)
(define-integer-writer %write-long! long #t)

(define-integer-reader %read-unsigned-int unsigned-int #f)
(define-integer-reader %read-unsigned-long unsigned-long #f)
(define-integer-writer %write-unsigned-int! unsigned-int #f)
(define-integer-writer %write-unsigned-long! unsigned-long #f)

(define-integer-reader %read-size_t size_t #f)
(define-integer-writer %write-size_t! size_t #f)

(define-integer-reader %read-pointer '* #f)
(define-integer-writer %write-pointer! '* #f)


(define *writers*
  `((,float . ,bytevector-ieee-single-native-set!)
    (,double . ,bytevector-ieee-double-native-set!)
    (,int8 . ,bytevector-s8-set!)
    (,uint8 . ,bytevector-u8-set!)
    (,int16 . ,bytevector-s16-native-set!)
    (,uint16 . ,bytevector-u16-native-set!)
    (,int32 . ,bytevector-s32-native-set!)
    (,uint32 . ,bytevector-u32-native-set!)
    (,int64 . ,bytevector-s64-native-set!)
    (,uint64 . ,bytevector-u64-native-set!)

    (,int           . ,%write-int!)
    (,unsigned-int  . ,%write-unsigned-int!)
    (,long          . ,%write-long!)
    (,unsigned-long . ,%write-unsigned-long!)
    (,size_t        . ,%write-size_t!)

    (*              . ,(lambda (bv offset ptr)
                         (%write-pointer! bv offset
                                          (pointer-address ptr))))))

(define *readers*
  `((,float . ,bytevector-ieee-single-native-ref)
    (,double . ,bytevector-ieee-double-native-ref)
    (,int8 . ,bytevector-s8-ref)
    (,uint8 . ,bytevector-u8-ref)
    (,int16 . ,bytevector-s16-native-ref)
    (,uint16 . ,bytevector-u16-native-ref)
    (,int32 . ,bytevector-s32-native-ref)
    (,uint32 . ,bytevector-u32-native-ref)
    (,int64 . ,bytevector-s64-native-ref)
    (,uint64 . ,bytevector-u64-native-ref)

    (,int           . ,%read-int)
    (,unsigned-int  . ,%read-unsigned-int)
    (,long          . ,%read-long)
    (,unsigned-long . ,%read-unsigned-long)
    (,size_t        . ,%read-size_t)

    (*              . ,(lambda (bv offset)
                         (make-pointer (%read-pointer bv offset))))))

(define (align off alignment)
  (1+ (logior (1- off) (1- alignment))))

(define (write-c-struct bv offset types vals)
  (let lp ((offset offset) (types types) (vals vals))
    (cond
     ((not (pair? types))
      (or (null? vals)
          (error "too many values" vals)))
     ((not (pair? vals))
      (error "too few values" types))
     (else
      ;; alignof will error-check
      (let* ((type (car types))
             (offset (align offset (alignof type))))
        (if (pair? type)
            (write-c-struct bv offset (car types) (car vals))
            ((assv-ref *writers* type) bv offset (car vals)))
        (lp (+ offset (sizeof type)) (cdr types) (cdr vals)))))))

(define (read-c-struct bv offset types)
  (let lp ((offset offset) (types types) (vals '()))
    (cond
     ((not (pair? types))
      (reverse vals))
     (else
      ;; alignof will error-check
      (let* ((type (car types))
             (offset (align offset (alignof type))))
        (lp (+ offset (sizeof type)) (cdr types)
            (cons (if (pair? type)
                      (read-c-struct bv offset (car types))
                      ((assv-ref *readers* type) bv offset))
                  vals)))))))

(define (make-c-struct types vals)
  (let ((bv (make-bytevector (sizeof types) 0)))
    (write-c-struct bv 0 types vals)
    (bytevector->pointer bv)))

(define (parse-c-struct foreign types)
  (let ((size (fold (lambda (type total)
                      (+ (sizeof type)
                         (align total (alignof type))))
                    0
                    types)))
    (read-c-struct (pointer->bytevector foreign size) 0 types)))
