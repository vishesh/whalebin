#lang web-server

(require web-server/servlet-env)
(require "model.rkt")
(require "config.rkt")

(define MIME-PLAIN-TEXT #"text/plain")

; TODO: Put constants and messages above

; response/message : String -> Response
(define (response/message msg)
  (response/xexpr
    `(html (head (title "Error!"))
           (body (p ,msg)))))

; do-dispatch : URL -> 
(define-values (do-dispatch url)
  (dispatch-rules
    [("api" "upload") #:method "post" serve-api-upload]
    [("get" (string-arg)) #:method "get" serve-get]
    [else serve-default]))

; serve-get : Request Name -> Response
(define (serve-get req name)
  (if (paste-exists? name)
    (if (paste-output-ready? name)
      (response/full
        200 #"OK"
        (current-seconds) TEXT/HTML-MIME-TYPE
        '()
        (list (port->bytes (paste-web-output name))))
      (response/message "Paste is not compiled yet! Try again in few seconds"))
    (response/message "Paste not found!")))

; serve-api-upload : Request -> Response
(define (serve-api-upload req)
  (define name (random-name NAME-LEN))
  (create-paste! name (request-post-data/raw req))
  (response/full
    200 #"OK"
    (current-seconds) MIME-PLAIN-TEXT
    '()
    (list (string->bytes/utf-8 name))))

; serve-default : Request -> Response
(define (serve-default req)
  (define (name->li name)
    `(li (a ((href ,(get-paste-url name))) ,name)))
  (response/xexpr
    `(html (head (title "whalebin!"))
           (body (p (h2 "whalebin : recent pastes") 
                   ,(append '(ul) (map name->li (get-recent-pastes))))))))

(define (get-paste-url name)
  (string-append "/get/" name))

(define (start req)
  (init-db)
  (do-dispatch req))

(serve/servlet start
               #:stateless? #t
               #:servlet-path "/"
               #:servlet-regexp #rx".*"
               #:servlet-current-directory ".")
