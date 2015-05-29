#lang racket

(require db
         "../utils.rkt"
         "init.rkt")

(provide create-user
         username?
         password?
         get-user-id
         get-username-by-id
         user-valid-cred?
         create-user-session
         destroy-session
         session-user)

; UserName is [a-zA-Z0-9_]{4,16}

; username? : Any -> Boolean
(define (username? a)
  (and (string? a)
       (<= 4 (string-length a) 16)
       (list? (regexp-match #rx"^[a-zA-Z0-9_]+$" a))))

; password? : Any -> Boolean
(define (password? p)
  (and (string? p)
       (<= 4 (string-length p) 20)
       (list? (regexp-match #rx"[a-zA-Z0-9_@#$%]+$" p))))

; create-user : String String -> Void
; WHERE: (get-user-id username) == false
(define (create-user username password)
  (when (and (username? username) (password? password))
    (query-exec
      DB-CONN
      (format "INSERT INTO ~a (username, password) VALUES (?, SHA1(?))" TABLE-USERS)
      username password)))

; String -> Maybe<Integer>
(define (get-user-id username)
  (query-maybe-value
    DB-CONN
    (format "SELECT id FROM ~a WHERE username = ?;" TABLE-USERS)
    username))

(define (get-username-by-id id)
  (query-maybe-value
    DB-CONN
    (format "SELECT username FROM ~a WHERE id = ?;" TABLE-USERS)
    id))

; String String -> Maybe<Integer>
; returns true if given credentials for user are valid
(define (user-valid-cred? username password)
  (query-maybe-value
    DB-CONN
    (format "SELECT id FROM ~a WHERE username = ? AND password = SHA1(?);"
            TABLE-USERS)
    username password))


;;;;;;;;;;;;; SESSIONS

; String -> String
(define (create-user-session username)
  (define userid (get-user-id username))
  (define token (random-string 64)) ; use uuid
  (query-exec
    DB-CONN
    (format "INSERT INTO ~a (user_id, token) VALUES (?, ?)" TABLE-SESSIONS)
    userid token)
  token)

(define (destroy-session token)
  (query-exec DB-CONN
              (format "DELETE FROM ~a WHERE token = ?;" TABLE-SESSIONS)
              token))

; Integer String -> Maybe<String>
(define (session-user token)
  (query-maybe-value
    DB-CONN
    (format "SELECT username FROM ~a, ~a WHERE token = ? AND
                    ~a.user_id = ~a.id;"
            TABLE-SESSIONS TABLE-USERS TABLE-SESSIONS TABLE-USERS)
    token))
