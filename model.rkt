#lang racket

(require db
         whalesong/whalesong-helpers)

(require "repo.rkt")
(require "config.rkt")

(provide init-db
         paste-source
         paste-exists?
         random-name
         create-paste!
         paste-output-ready?
         get-recent-pastes
         paste-web-output)

;; Database 

; TODO: use connection pool
; http://docs.racket-lang.org/db/using-db.html#%28part._intro-servlets%29
(define dbconn
  (mysql-connect #:user "racket"
                 #:database "bigbang"
                 #:password "racket"))

(define TABLE-PASTES "pastes")
(define TABLE-WORKER "worker_queue")
(define CHARS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

; random-name : Integer -> Name
; Returns a random name using random-string that has not been used by any
; paste. Wont halt if all names ore consumed, so choose a reasonable len
(define (random-name len)
  (let loop ([name (random-string len)])
    (if (paste-exists? name)
      (loop)
      name)))

; random-string : Integer -> Name
; Returns a random string of length len with chars from CHAR
(define (random-string len)
  (define lc (string-length CHARS))
  (list->string (for/list ([i (range len)])
                          (string-ref CHARS (random lc)))))

; init-db : -> Void
(define (init-db)
  (unless (table-exists? dbconn TABLE-PASTES)
    (query-exec dbconn
                (format "CREATE TABLE ~a (
                           id INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT,
                           name VARCHAR(16) NOT NULL UNIQUE,
                           ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                         );" TABLE-PASTES)))
     (unless (table-exists? dbconn TABLE-WORKER)
       (query-exec dbconn
                (format "CREATE TABLE ~a (paste_id INTEGER NOT NULL);"
                        TABLE-WORKER))))

; get-paste-id : Name -> Integer
; Get ID of paste tuple with name
(define (get-paste-id name)
  (query-maybe-value
    dbconn
    (format "SELECT id FROM ~a WHERE name = ?" TABLE-PASTES)
    name))

; paste-exists? : Name -> Boolean
(define (paste-exists? name)
  (not (false? (get-paste-id name))))

; paste-source : Name -> Port
(define (paste-source name)
  (repo-open-input-file REPO-SOURCE name))

; paste-web-output : Name -> Port
(define (paste-web-output name)
  (repo-open-input-file REPO-OUTPUT name))

; get-recent-pastes : -> ListOf<Name>
(define (get-recent-pastes)
  (query-list
    dbconn
    (format "SELECT name FROM ~a ORDER BY ts DESC LIMIT 10" TABLE-PASTES)))

; paste-output-ready? : Name -> Boolean
(define (paste-output-ready? name)
  (define paste-id (get-paste-id name))
  (if paste-id
    (not (query-maybe-value
           dbconn
           (format "SELECT paste_id FROM ~a WHERE paste_id = ?" TABLE-WORKER)
           paste-id))
    #t))

; create-paste! : Name bytes -> Void
; TODO: put both queries in transaction
(define (create-paste! name content)
  (query-exec dbconn
              (format "INSERT INTO ~a (name) VALUES (?)" TABLE-PASTES)
              name)
  (query-exec dbconn
              (format "INSERT INTO ~a (paste_id) VALUES (?)" TABLE-WORKER)
              (get-paste-id name))
  (repo-put! REPO-SOURCE name content)
  (compile-source name))


; finish-compile : Name -> Void
(define (finish-compile name)
  (define paste-id (get-paste-id name))
  (query-exec dbconn
              (format "DELETE FROM ~a WHERE paste_id = ?" TABLE-WORKER)
              paste-id))

; compile-source : Name -> Void
; Spawns a new thread to compile whalesong program
; TODO: call finish-compile only if build was successful
(define (compile-source name)
  (thread
    (λ ()
      (parameterize ([current-output-dir
                      (path-only (repo-filepath REPO-OUTPUT name))])
                    (build-standalone-html (repo-filepath REPO-SOURCE name)))
      (finish-compile name))))
