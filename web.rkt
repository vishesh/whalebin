#lang racket

(require net/uri-codec
         web-server/http
         web-server/http/id-cookie
         (for-syntax syntax/parse))

(require "model/user.rkt"
         "config.rkt")

(provide current-session
         request-session-cookie
         define/session-handler)

;; Session helpers

(define current-session (make-parameter #f))

(define (request-session-cookie req)
  (request-id-cookie SESSION-COOKIE-NAME SESSION-COOKIE-SALT req))

(define (get-current-session req)
  (define token (request-session-cookie req))
  (hash 'username (and token
                       (session-user token))))

(define-syntax define/session-handler
  (syntax-rules ()
    [(_ (id req args ...) body0 body ...)
     (define (id req args ...)
       (parameterize ([current-session (get-current-session req)])
         body0 body ...))]))
