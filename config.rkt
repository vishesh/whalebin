#lang racket

(require (only-in web-server/http/id-cookie
                  make-secret-salt/file)
         (prefix-in es: elasticsearch))

(provide (all-defined-out))

; Constants

(define SERVER-ROOT "http://localhost:8000")
(define REPO-SUBDIR-NAME-LEN 2)
(define NAME-LEN 5)

(define REPO-SOURCE "data/src")
(define REPO-OUTPUT "data/outputs")

(define DB-USER "racket")
(define DB-PASSWORD "racket")
(define DB-NAME "bigbang")

(define SESSION-COOKIE-NAME "session") 
(define SESSION-COOKIE-SALT (make-secret-salt/file "SESSION-SALT"))

(define STATIC-FILES-DIR "/home/vishesh/racket/whalebin/static")

; elasticsearch settings
(define INDEX-NAME "whalebin")
(define DOCUMENT-NAME "paste")
(define ES-HOST "localhost")

(define ES-CLIENT es:DEFAULT-CLIENT) ; FIXME: put this in config with app

