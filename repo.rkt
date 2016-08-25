#lang racket

(require "config.rkt")

(provide repo-filepath
         repo-put!)

; Repository is a Path
; interp: Root directory path of repository

; Name is a String, matches [a-zA-Z0-9]+
; interp: represents unique URL name

; repo-filepath : Repository Name -> Path
(define (repo-filepath repo name)
  (build-path repo (substring name 0 REPO-SUBDIR-NAME-LEN) name))

; repo-put! : Repository Name bytes -> Void
(define (repo-put! repo name content)
  (define basedir (path-only (repo-filepath repo name)))

  (unless (directory-exists? basedir)
    (make-directory basedir))

  (define filepath (repo-filepath repo name))
  (call-with-output-file filepath #:exists 'truncate
    (λ (out)
      (write-bytes content out))))
