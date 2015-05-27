#lang racket

(require web-server/http
         web-server/http/bindings
         web-server/http/id-cookie
         web-server/servlet/web
         web-server/servlet-env
         web-server/dispatch)
(require "model.rkt")
(require "config.rkt")

(define MIME-PLAIN-TEXT #"text/plain")

; TODO: Put constants and messages above

; response/message : String -> Response
(define-syntax-rule (response/message msg)
  (response/xexpr
    `(html (head (title "whalebin : message"))
           (body (p ,msg)))))

; do-dispatch : URL -> 
(define-values (do-dispatch url)
  (dispatch-rules
    [("api" "upload") #:method "post" serve-api-upload]
    [("upload") #:method "post" serve-upload]
    [("get" (string-arg)) #:method "get" serve-get]
    [("auth" "signin") #:method "get" serve-signin]
    [("auth" "signup") #:method "get" serve-signup]
    [("auth" "signoff") #:method "get" serve-signoff]
    [("profile" (string-arg)) #:method "get" serve-profile]
    [else serve-default]))


(define (request-session-token req)
  (request-id-cookie SESSION-COOKIE-NAME SESSION-COOKIE-SALT req))

(define (get-session-username req)
  (define token (request-session-token req))
  (and token
       (session-user token)))

(define (logged-in? req)
  (string? (get-session-username req)))

; serve-get : Request Name -> Response
(define (serve-get req name)
  (if (paste-exists? name)
    (cond
      [(paste-output-ready? name)
       (paste-views-add1 name)
       (response/full
         200 #"OK"
         (current-seconds) TEXT/HTML-MIME-TYPE
         '()
         (list (port->bytes (paste-web-output name))))]
      [else (response/message "Paste is not compiled yet! Try again in few seconds")])
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
  (define session-user (let ([username (get-session-username req)])
                         (and username
                              (get-user-id username))))
  (cond
    [(exists-binding? 'source bindings)
     (create-paste!
      name
      (string->bytes/utf-8 (extract-binding/single 'source bindings))
      session-user)
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
  (define session-user (get-session-username req))
  (define (name->li name)
    `(li (a ([href ,(get-paste-url name)]) ,name)))
  (response/xexpr
    `(html (head (title "whalebin!"))
           (body
             (h2 "whalebin")
             (div ([style "margin-bottom: 1em; font-size: 80%; font-weight: bold;"])
                  ,(if session-user
                     (user-bar-xexpr session-user)
                     (guest-bar-xexpr)))
             (div ([style "float: left; width: 200px;"])
                  (yh4 "recent pastes")
                  (ul ,@(map name->li (get-recent-pastes))))
             (div ((style "float: left; width: 300px;"))
                  (form ([method "post"] [action "/upload"])
                        (textarea ([name "source"] [cols "80"] [rows "30"]))
                        (input ([type "submit"] [value "Upload"]))))))))

; serve-signup : Request -> Response
(define (serve-signup req)
  (define (get-creds)
    (define req
      (send/suspend
        (λ (k-url)
          (response/xexpr 
            `(html (head (title "whalebing : signup"))
                   (body
                     (h2 "whalebin : signup")
                     (div
                       (form ([method "post"] [action ,k-url])
                             "Username"
                             (input ([type "text"] [name "username"]))
                             "Password"
                             (input ([type "password"] [name "password"]))
                             (input ([type "submit"]))))))))))
    (cons (extract-binding/single 'username (request-bindings req))
          (extract-binding/single 'password (request-bindings req))))

  (response/message
    (match (get-creds)
      [(cons username password)
       (cond
         [(not (get-user-id username)) (create-user username password)
                                        "user created. go back and login"]
         [else "user already exists. Try another username"])])))

(define (serve-signin req)
  (define (get-creds)
    (define req
      (send/suspend
        (λ (k-url)
          (response/xexpr 
            `(html (head (title "whalebing : signin"))
                   (body
                     (h2 "whalebin : signin")
                     (div
                       (form ([method "post"] [action ,k-url])
                             "Username"
                             (input ([type "text"] [name "username"]))
                             "Password"
                             (input ([type "password"] [name "password"]))
                             (input ([type "submit"]))))))))))
    (cons (extract-binding/single 'username (request-bindings req))
          (extract-binding/single 'password (request-bindings req))))

  (match (get-creds)
    [(cons username password)
     (cond
       [(user-valid-cred? username password)
        (define cookie (make-id-cookie SESSION-COOKIE-NAME
                                       SESSION-COOKIE-SALT
                                       (create-user-session username)
                                       #:path "/"))
        (redirect-to "/"
                     #:headers (map cookie->header (list cookie)))]
       [else (response/message "invalid username/password.")])]))

(define (serve-signoff req)
  (destroy-session (request-session-token req))
  (redirect-to "/"
               #:headers
               (map cookie->header
                    (list (logout-id-cookie SESSION-COOKIE-NAME #:path "/")))))

(define (serve-profile req username)
  (define userid (get-user-id username))
  (define (name->li name)
    `(li (a ([href ,(get-paste-url name)]) ,name)))
  (response/xexpr
    `(html (head (title "whalebin!"))
           (body
             (h2 "whalebin")
             (h5 "profile: " ,username
             (div 
               (ul ,@(map name->li (get-user-pastes userid)))))))))

; get-paste-url : Name -> String
(define (get-paste-url name)
  (string-append SERVER-ROOT "/get/" name))

(define (user-bar-xexpr user)
  `(div
     (string-append "Welcome " ,user)
     " ( "
     (a ([href "/auth/signoff"]) "Signoff")
     " ) "))

(define (guest-bar-xexpr)
  `(div
     "Welcome Guest!"
     " ( "
     (a ([href "/auth/signin"]) "Signin") " | "
     (a ([href "/auth/signup"]) "Signup")
     " ) "))

(define (start req)
  (do-dispatch req))

(define (main)
  (init-db)
  (serve/servlet start
                 #:servlet-path "/"
                 #:servlet-regexp #rx".*"
                 #:servlet-current-directory "."
                 #:launch-browser? false))

; run
(main)
