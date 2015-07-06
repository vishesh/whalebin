#lang racket

(provide random-string
         hash-refs)

(define CHARS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

; random-string : Integer -> Name
; Returns a random string of length len with chars from CHAR
(define (random-string len)
  (define lc (string-length CHARS))
  (list->string (for/list ([i (range len)])
                          (string-ref CHARS (random lc)))))

(define-syntax (hash-refs stx)
  (syntax-case stx ()
    [(_ h f)
     #'(hash-ref h f)]
    [(_ h f r ...)
     #'(hash-refs (hash-ref h f) r ...)]))
