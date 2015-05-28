#lang racket

(require db
         whalesong/whalesong-helpers 
         "../repo.rkt"
         "../config.rkt"
         "../utils.rkt"
         "init.rkt")

(provide random-name
         get-paste-id
         paste-exists?
         paste-source
         paste-web-output
         get-recent-pastes
         paste-output-ready?
         create-paste!
         finish-compile
         paste-views-add1
         get-user-pastes)

; Name is [a-zA-Z0-9]+

; random-name : Integer -> Name
; Returns a random name using random-string that has not been used by any
; paste. Wont halt if all names ore consumed, so choose a reasonable len
(define (random-name len)
  (let loop ([name (random-string len)])
    (if (paste-exists? name)
      (loop)
      name)))

; get-paste-id : Name -> Maybe<Integer>
; Get ID of paste tuple with name
(define (get-paste-id name)
  (query-maybe-value
    DB-CONN
    (format "SELECT id FROM ~a WHERE name = ?" TABLE-PASTES)
    name))

; paste-exists? : Name -> Boolean
(define (paste-exists? name)
  (not (false? (get-paste-id name))))

; paste-source : Name -> Port
; Source code of given paste
(define (paste-source name)
  (repo-open-input-file REPO-SOURCE name))

; paste-web-output : Name -> Port
; HTML output of given paste
(define (paste-web-output name)
  (repo-open-input-file REPO-OUTPUT name))

; get-recent-pastes : -> ListOf<Name>
; Get 10 recently added pastes
(define (get-recent-pastes)
  (query-list
    DB-CONN
    (format "SELECT name FROM ~a ORDER BY ts DESC LIMIT 10" TABLE-PASTES)))

; paste-output-ready? : Name -> Boolean
; WHERE: (paste-exists? name) is true
; Returns true if the program has compiled and is ready to be fetched
(define (paste-output-ready? name)
  (not (query-maybe-value
         DB-CONN
         (format "SELECT paste_id FROM ~a WHERE paste_id = ?" TABLE-WORKER)
         (get-paste-id name))))

; create-paste! : Name bytes -> Void
; TODO: put both queries in transaction
(define (create-paste! name content [title ""] [description ""] [userid #f])
  (query-exec
    DB-CONN
    (format "INSERT INTO ~a (name, user_id, title, desc) VALUES (?, ?, ?, ?)"
            TABLE-PASTES)
    name userid title description)
  (query-exec
    DB-CONN
    (format "INSERT INTO ~a (paste_id) VALUES (?)" TABLE-WORKER)
    (get-paste-id name))
  (repo-put! REPO-SOURCE name content)
  (compile-source name))

; finish-compile : Name -> Void
; Mark the paste as compiled and ready by removed it from worker queue
; TODO: Fails if compilation fails
(define (finish-compile name)
  (query-exec
    DB-CONN
    (format "DELETE FROM ~a WHERE paste_id = ?" TABLE-WORKER)
    (get-paste-id name)))

; Name -> Void
(define (paste-views-add1 name)
  (query-exec
    DB-CONN
    (format "UPDATE ~a SET views = views + 1 WHERE id = ?" TABLE-PASTES)
    (get-paste-id name)))

; get-user-pastes : Integer -> ListOf<Name>
; Returns list of pastes by user
(define (get-user-pastes userid)
  (query-list
    DB-CONN
    (format "SELECT name FROM ~a WHERE id =? ORDER BY ts DESC" TABLE-PASTES)
    userid))

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
