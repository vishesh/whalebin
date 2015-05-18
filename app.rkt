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
    [("upload") #:method "post" serve-upload]
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
    (list (string->bytes/utf-8 (get-paste-url name)))))

; serve-upload : Request -> Response
(define (serve-upload req)
  (define name (random-name NAME-LEN))
  (define bindings (request-bindings req))
  (cond
    [(exists-binding? 'source bindings)
     (create-paste!
      name (string->bytes/utf-8 (extract-binding/single 'source bindings)))
     (response/xexpr
      `(html
        (head (title "whalebin : upload"))
        (body
         (h2 "source uploaded")
         (p ,(string-append
              "Program successfully uploaded. Compilation can take "
              "few seconds to complete. ")
            (a ([href ,(get-paste-url name)]) ,(get-paste-url name))))))]
    [else (response/message "Invalid params passed")]))

; serve-default : Request -> Response
(define (serve-default req)
  (define (name->li name)
    `(li (a ([href ,(get-paste-url name)]) ,name)))
  (response/xexpr
    `(html (head (title "whalebin!"))
           (body
             (h2 "whalebin")
             (div ([style "float: left; width: 200px;"])
                  (h4 "recent pastes")
                  (ul ,@(map name->li (get-recent-pastes))))
             (div ((style "float: left; width: 300px;"))
                  (form ([method "post"] [action "/upload"])
                        (textarea ([name "source"] [cols "80"] [rows "30"]))
                        (input ([type "submit"] [value "Upload"]))))))))

; get-paste-url : Name -> String
(define (get-paste-url name)
  (string-append SERVER-ROOT "/get/" name))

(define (start req)
  (init-db)
  (do-dispatch req))

(serve/servlet start
               #:stateless? #t
               #:servlet-path "/"
               #:servlet-regexp #rx".*"
               #:servlet-current-directory "."
               #:launch-browser? false)
