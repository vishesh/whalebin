#lang racket

(require db)
(require "../config.rkt")

(provide (all-defined-out))

(define TABLE-PASTES "pastes")
(define TABLE-WORKER "worker_queue")
(define TABLE-USERS "users")
(define TABLE-SESSIONS "sessions")

; TODO: use connection pool
; http://docs.racket-lang.org/db/using-db.html#%28part._intro-servlets%29
(define DB-CONN
  (virtual-connection
    (connection-pool
      (λ () (mysql-connect #:user DB-USER
                        #:database DB-NAME
                        #:password DB-PASSWORD)))))

; init-db : -> Void
(define (init-db)
  (unless (table-exists? DB-CONN TABLE-USERS)
    (query-exec DB-CONN
                (format "CREATE TABLE ~a (
                           id INTEGER NOT NULL AUTO_INCREMENT,
                           username VARCHAR(16) NOT NULL UNIQUE,
                           password VARCHAR(40) NOT NULL,
                           PRIMARY KEY (id)
                         );" TABLE-USERS)))
  (unless (table-exists? DB-CONN TABLE-SESSIONS)
    (query-exec DB-CONN
                (format "CREATE TABLE ~a (
                           id INTEGER NOT NULL AUTO_INCREMENT,
                           user_id INTEGER,
                           token VARCHAR(64) NOT NULL,
                           ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                           PRIMARY KEY (id),
                           FOREIGN KEY (user_id) REFERENCES ~a (id)
                         );" TABLE-SESSIONS TABLE-USERS)))
  (unless (table-exists? DB-CONN TABLE-PASTES)
    (query-exec DB-CONN
                (format "CREATE TABLE ~a (
                           id INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT,
                           url VARCHAR(16) NOT NULL UNIQUE,
                           title VARCHAR(20) NOT NULL DEFAULT '',
                           descp VARCHAR (255) NOT NULL DEFAULT '', 
                           create_ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                           last_ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                           compiler_error BOOLEAN,
                           user_id INTEGER,
                           private BOOLEAN NOT NULL,
                           views INTEGER NOT NULL DEFAULT 0,
                           FOREIGN KEY (user_id) REFERENCES ~a (id)
                         );" TABLE-PASTES TABLE-USERS))
    (query-exec DB-CONN
        (format "ALTER TABLE ~a AUTO_INCREMENT = 1000;"
                TABLE-PASTES)))
  (unless (table-exists? DB-CONN TABLE-WORKER)
    (query-exec DB-CONN
             (format "CREATE TABLE ~a (paste_id INTEGER NOT NULL);"
                     TABLE-WORKER))))
