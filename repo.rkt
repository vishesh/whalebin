#lang racket

(require "config.rkt")

(provide repo-open-input-file
         repo-open-output-file
         repo-filepath
         repo-put!)

; Repository is a Path
; interp: Root directory path of repository

; Name is a String, matches [a-zA-Z0-9]+
; interp: represents unique URL name

; repo-filepath : Repository Name -> Path
(define (repo-filepath repo name)
  (build-path repo (substring name 0 REPO-SUBDIR-NAME-LEN) name))

; repo-open-input-file : Repository Name -> Port
(define (repo-open-input-file repo name)
  (open-input-file (repo-filepath repo name)))

; repo-open-output-file : Repository Name -> Port
(define (repo-open-output-file repo name)
  (define basedir (path-only (repo-filepath repo name)))
  (unless (directory-exists? basedir)
    (make-directory basedir))
  (open-output-file (repo-filepath repo name)))

; repo-put! : Repository Name bytes -> Void
(define (repo-put! repo name content)
  (define port (repo-open-output-file repo name))
  (write-bytes content port)
  (close-output-port port))
