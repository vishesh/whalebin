#lang racket

(require db
         db/util/datetime
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
         paste-starred-by-user?
         paste-star-by-user
         paste-unstar-by-user
         paste-star-counts
         get-paste-by-name
         get-all-pastes
         get-recent-pastes
         get-top-starred-pastes
         get-user-starred-pastes
         get-most-viewed-pastes
         paste-output-ready?
         create-paste!
         finish-compile
         paste-views-add1
         get-user-pastes
         (struct-out paste)
         make-paste
         paste-save!)

; Name is [a-zA-Z0-9]+

(define-struct paste (id url title descp create-ts last-ts userid views private? compiler-error?) #:transparent)

(define PASTE-ROW-NAMES #(id url title descp create_ts last_ts user_id views private compiler_error))

(define (result->paste result)
  (define rh (make-immutable-hash (vector->list result)))
  (make-paste (hash-ref rh 'id)
              (hash-ref rh 'url)
              (hash-ref rh 'title)
              (hash-ref rh 'descp)
              (sql-datetime->srfi-date (hash-ref rh 'create_ts))
              (sql-datetime->srfi-date (hash-ref rh 'last_ts))
              (hash-ref rh 'user_id)
              (hash-ref rh 'views)
              (hash-ref rh 'private )
              (hash-ref rh 'compiler_error)))

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

; paste-save! : Paste -> Void
(define (paste-save! paste source)
  (define source/utf-8 (string->bytes/utf-8 source))
  (query-exec
    DB-CONN
    (format  #<<EOF
             UPDATE ~a SET
                title = ?,
                descp = ?,
                last_ts = ?,
                user_id = ?,
                views = ?,
                private = ?,
                compiler_error = ?
            WHERE id = ?;
EOF
    TABLE-PASTES)
    (paste-title paste)
    (paste-descp paste) 
    (srfi-date->sql-timestamp (paste-last-ts paste))
    (paste-userid paste)
    (paste-views paste)
    (paste-private? paste)
    (paste-compiler-error? paste)
    (paste-id paste))
  (when (not (equal? source/utf-8 (port->bytes (paste-source paste))))
    (query-exec
      DB-CONN
      (format "INSERT INTO ~a (paste_id) VALUES (?)" TABLE-WORKER)
      (get-paste-id (paste-url paste)))
    (repo-put! REPO-SOURCE (paste-url paste) source/utf-8)
    (compile-source (paste-url paste))))

; get-paste : Name -> Maybe<Paste>
(define (get-paste-by-name url)
  (define result 
    (query-maybe-row
      DB-CONN
      (format "SELECT id, url, title, descp, create_ts, last_ts, user_id, views, private, compiler_error FROM ~a WHERE url = ?" TABLE-PASTES)
      url))
  (and result
       (result->paste (vector-map cons PASTE-ROW-NAMES result))))

(define (get-paste-by-id id)
  (define result 
    (query-maybe-row
      DB-CONN
      (format "SELECT id, url, title, descp, create_ts, last_ts, user_id, views, private, compiler_error FROM ~a WHERE id = ?" TABLE-PASTES)
      id))
  (and result
       (result->paste (vector-map cons PASTE-ROW-NAMES result))))

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
      (format "SELECT id, url, title, descp, create_ts, last_ts, user_id, views, private, compiler_error FROM ~a ORDER BY create_ts DESC LIMIT ?" TABLE-PASTES) n))
  (map (lambda (x)
         (result->paste (vector-map cons PASTE-ROW-NAMES x)))
       results))

; get-recent-pastes : Integer -> ListOf<Paste>
; Get n most viewed pastes
(define (get-most-viewed-pastes n)
  (define results
    (query-rows
      DB-CONN
      (format "SELECT id, url, title, descp, create_ts, last_ts, user_id, views, private, compiler_error FROM ~a ORDER BY views DESC LIMIT ?" TABLE-PASTES) n))
  (map (lambda (x)
         (result->paste (vector-map cons PASTE-ROW-NAMES x)))
       results))

; get-top-starred-pastes : Integer -> ListOf<Paste>
; Get n most viewed pastes
(define (get-top-starred-pastes n)
  (define results
    (query-rows
      DB-CONN
      (format "SELECT id, url, title, descp, create_ts, last_ts, user_id, views, private, compiler_error FROM ~a WHERE id IN
              (SELECT paste_id FROM ~a GROUP BY paste_id ORDER BY count(paste_id) DESC) LIMIT ?" TABLE-PASTES TABLE-STARS) n))
  (map (lambda (x)
         (result->paste (vector-map cons PASTE-ROW-NAMES x)))
       results))

; get-user-starred-pastes : UserId -> ListOf<Paste>
; Get n most viewed pastes
(define (get-user-starred-pastes userid)
  (define results
    (query-rows
      DB-CONN
      (format "SELECT id, url, title, descp, create_ts, last_ts, user_id, views, private, compiler_error FROM ~a WHERE id IN
              (SELECT paste_id FROM ~a WHERE user_id = ? ORDER BY ts DESC)" TABLE-PASTES TABLE-STARS) userid))
  (map (lambda (x)
         (result->paste (vector-map cons PASTE-ROW-NAMES x)))
       results))

; paste-output-ready? : Paste -> Boolean
; Returns true if the program has compiled and is ready to be fetched
(define (paste-output-ready? paste)
  (not (query-maybe-value
         DB-CONN
         (format "SELECT paste_id FROM ~a WHERE paste_id = ?" TABLE-WORKER)
         (paste-id paste))))

; paste-star-counts : Paste -> PosInt
(define (paste-star-counts paste)
  (query-maybe-value
    DB-CONN
    (format "SELECT COUNT(*) FROM ~a WHERE paste_id = ?" TABLE-STARS)
    (paste-id paste)))

(define (paste-starred-by-user? paste user-id)
  (and user-id
       (not 
         (false?
           (query-maybe-value
             DB-CONN
             (format "SELECT id FROM ~a WHERE paste_id = ? AND user_id = ?" TABLE-STARS)
             (paste-id paste)
             user-id)))))

(define (paste-star-by-user paste user_id)
  (when user_id
    (call-with-transaction
      DB-CONN
      (λ ()
        (when (not (paste-starred-by-user? paste user_id))
          (query-exec
            DB-CONN
            (format "INSERT INTO ~a (paste_id, user_id) VALUES (?, ?)" TABLE-STARS)
            (paste-id paste)
            user_id))))))

(define (paste-unstar-by-user paste user_id)
  (when user_id
    (query-exec
      DB-CONN
      (format "DELETE FROM ~a WHERE paste_id = ? AND user_id = ?" TABLE-STARS)
      (paste-id paste)
      user_id)))

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

; get-all-pastes : -> ListOf<Paste>
(define (get-all-pastes)
  (define result
    (query-rows
      DB-CONN
      (format "SELECT id, url, title, descp, create_ts, last_ts, user_id, views, private, compiler_error FROM ~a" TABLE-PASTES)))
  (map (λ (x)
         (result->paste (vector-map cons PASTE-ROW-NAMES x)))
       result))

; get-user-pastes : Integer -> ListOf<Name>
; Returns list of pastes by user
(define (get-user-pastes userid)
  (define results
    (query-rows
      DB-CONN
      (format "SELECT id, url, title, descp, create_ts, last_ts, user_id, views, private, compiler_error FROM ~a WHERE user_id = ? ORDER BY create_ts DESC" TABLE-PASTES)
      userid))
  (map (lambda (x)
         (result->paste (vector-map cons PASTE-ROW-NAMES x)))
       results))

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
