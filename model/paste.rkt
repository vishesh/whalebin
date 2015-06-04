#lang racket

(require db
         xml
         whalesong/whalesong-helpers 
         whalesong/parameters
         "../repo.rkt"
         "../config.rkt"
         "../utils.rkt"
         "init.rkt")

(provide random-name
         get-paste-id
         paste-exists?
         paste-source
         paste-web-output
         get-paste-by-name
         get-recent-pastes
         get-most-viewed-pastes
         paste-output-ready?
         create-paste!
         finish-compile
         paste-views-add1
         get-user-pastes
         paste-url
         paste-id
         paste-title
         paste-ts
         paste-userid
         paste-views
         paste-compiler-error?
         paste-descp
         paste-private?)

; Name is [a-zA-Z0-9]+

(define-struct paste (id url title descp ts userid views private? compiler-error?) #:transparent)

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
(define (get-paste-id url)
  (query-maybe-value
    DB-CONN
    (format "SELECT id FROM ~a WHERE url = ?" TABLE-PASTES)
    url))

; get-paste : Name -> Maybe<Paste>
(define (get-paste-by-name url)
  (define result 
    (query-maybe-row
      DB-CONN
      (format "SELECT id, url, title, descp, ts, user_id, views, private, compiler_error FROM ~a WHERE url = ?" TABLE-PASTES)
      url))
  (and result
       (apply make-paste (vector->list result))))

(define (get-paste-by-id id)
  (define result 
    (query-maybe-row
      DB-CONN
      (format "SELECT id, url, title, descp, ts, user_id, views, private, compiler_error FROM ~a WHERE id = ?" TABLE-PASTES)
      id))
  (and result
       (apply make-paste (vector->list result))))

; paste-exists? : Name -> Boolean
(define (paste-exists? url)
  (not (false? (get-paste-id url))))

; paste-source : Paste -> Port
; Source code of given paste
(define (paste-source paste)
  (repo-open-input-file REPO-SOURCE (paste-url paste)))

; paste-web-output : Paste -> Port
; HTML output of given paste
(define (paste-web-output paste)
  (repo-open-input-file REPO-OUTPUT (paste-url paste)))

; TODO
; get-recent-pastes : Integer -> ListOf<Paste>
; Get n recently added paste
(define (get-recent-pastes n)
  (define results
    (query-rows
      DB-CONN
      (format "SELECT id, url, title, descp, ts, user_id, views, private, compiler_error FROM ~a ORDER BY ts DESC LIMIT ?" TABLE-PASTES) n))
  (map (lambda (x) (apply make-paste (vector->list x))) results))

; get-recent-pastes : Integer -> ListOf<Paste>
; Get n most viewed pastes
(define (get-most-viewed-pastes n)
  (define results
    (query-rows
      DB-CONN
      (format "SELECT id, url, title, descp, ts, user_id, views, private, compiler_error FROM ~a ORDER BY views DESC LIMIT ?" TABLE-PASTES) n))
  (map (lambda (x) (apply make-paste (vector->list x))) results))

; paste-output-ready? : Paste -> Boolean
; WHERE: (paste-exists? name) is true
; Returns true if the program has compiled and is ready to be fetched
(define (paste-output-ready? paste)
  (not (query-maybe-value
         DB-CONN
         (format "SELECT paste_id FROM ~a WHERE paste_id = ?" TABLE-WORKER)
         (paste-id paste))))

; create-paste! : Name bytes -> Void
; TODO: put both queries in transaction
(define (create-paste! url content #:title [title ""] #:descp [descp ""] #:userid [userid #f] #:private [private #f])
  (query-exec
    DB-CONN
    (format "INSERT INTO ~a (url, user_id, title, descp, private) VALUES (?, ?, ?, ?, ?)"
            TABLE-PASTES)
    url (false->sql-null userid) title descp (if private 1 0))
  (query-exec
    DB-CONN
    (format "INSERT INTO ~a (paste_id) VALUES (?)" TABLE-WORKER)
    (get-paste-id url))
  (repo-put! REPO-SOURCE url content)
  (compile-source url))

; finish-compile : Name -> Void
; Mark the paste as compiled and ready by removed it from worker queue
; TODO: Fails if compilation fails
(define (finish-compile url)
  (query-exec
    DB-CONN
    (format "DELETE FROM ~a WHERE paste_id = ?" TABLE-WORKER)
    (get-paste-id url)))

; Name -> Void
(define (paste-views-add1 paste)
  (query-exec
    DB-CONN
    (format "UPDATE ~a SET views = views + 1 WHERE id = ?" TABLE-PASTES)
    (paste-id paste)))

; get-user-pastes : Integer -> ListOf<Name>
; Returns list of pastes by user
(define (get-user-pastes userid)
  (define results
    (query-rows
      DB-CONN
      (format "SELECT id, url, title, descp, ts, user_id, views, private, compiler_error FROM ~a WHERE user_id = ? ORDER BY ts DESC" TABLE-PASTES)
      userid))
  (map (lambda (x) (apply make-paste (vector->list x))) results))

; check-error-report : Name String -> Void
(define (check-error-report name r)
  (when (regexp-match "ERROR:" r)
    (query-exec
      DB-CONN
      (format "UPDATE ~a SET compiler_error = 1 WHERE url = ?" TABLE-PASTES)
      name)
    (repo-put! REPO-OUTPUT name 
               (string->bytes/utf-8 
                 (xexpr->string `(html
                                   (head (title "whalesong : error"))
                                   (body
                                     (h2 ,(format "compile error (~a)" name))
                                     (pre ,r))))))))

; compile-source : Name -> Void
; Spawns a new thread to compile whalesong program
; TODO: call finish-compile only if build was successful
(define (compile-source name)
  (thread
    (λ ()
      (define s (open-output-string))
      (parameterize ([current-output-dir
                      (path-only (repo-filepath REPO-OUTPUT name))]
                     [current-report-port s])
        (build-standalone-html (repo-filepath REPO-SOURCE name))
        (check-error-report name (get-output-string s)))
      (finish-compile name))))
