#lang racket

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
(define SESSION-COOKIE-SALT #"abcdefgijklmonp")

(define STATIC-FILES-DIR "/home/vishesh/racket/whalebin/static")
