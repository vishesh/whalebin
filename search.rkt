#lang racket

(require (prefix-in es: elasticsearch)
         "model.rkt"
         "utils.rkt"
         "config.rkt")

(provide search-hits->xexpr
         search-paste
         index-all)

(define ES-CLIENT es:DEFAULT-CLIENT) ; FIXME: put this in config with app

;;;FIXME: following functions are duplicated from app.py
; get-paste-url : Name -> String
(define (get-paste-url name)
  (string-append SERVER-ROOT "/get/" name))

; get-paste-source-url : Name -> String
(define (get-paste-source-url name)
  (string-append SERVER-ROOT "/get-src/" name))

; paste -> string
; returns title if available or the paste number for displaying
(define (paste-friendly-title paste)
  (if (zero? (string-length (paste-title paste)))
    (paste-url paste)
    (paste-title paste)))

; search-hits->xexpr : jsexpr? -> xexpr?
(define (search-hits->xexpr hits)
  (for/list ([hit hits])
    (define paste (get-paste-by-name (hash-refs hit '_source 'url)))
    `(div ([class "result-row"])
       (h4 (a ([href ,(get-paste-url (paste-url paste))])
              ,(paste-friendly-title paste)) 
           " ["
           (a ([href ,(get-paste-source-url (paste-url paste))]) "src")
           "]")
       (p ,(paste-descp paste))
       (hr))))

(define (search-paste q)
  (es:query-string ES-CLIENT q #:index INDEX-NAME #:doctype DOCUMENT-NAME))

(define (index-all)
  (define pastes (get-all-pastes))
  (for ([paste pastes])
    (es:document-index ES-CLIENT INDEX-NAME DOCUMENT-NAME
                       (hash 'url (paste-url paste)
                             'title (paste-title paste)
                             'description (paste-descp paste)))))

