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

(define (profile-url user)
  (string-append "/profile/" user))

(define (header-template user)
  `(nav ([class "navbar navbar-inverse navbar-fixed-top"])
        (div ([class "container"])
             (div ([class "navbar-header"])
                  (a ([class "navbar-brand" href="#"]) "Whalebin"))
             (div ([id "navbar"])
                  (ul ([class "nav navbar-nav"])
                      (li (a ([href "/"]) "New"))
                      (li (a ([href "/explore"]) "Explore")))
                  (ul ([class "nav navbar-nav navbar-right"])
                      ,@(if user
                          (list 
                            `(li (a ([href ,(profile-url user)]) ,user))
                            `(li (a ([href "/auth/signoff"]) "Sign Off")))
                          (list `(li (a ([href "/auth/signin"]) "Sign In"))
                                `(li (a ([href "/auth/signup"]) "Register")))))))))

(define (page-template title user body)
  `(html (head
           (title ,(string-append "whalebin " title))
           (meta ([charset "utf-8"]))
           (link ([rel "stylesheet"]
                  [type "text/css"]
                  [href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"])) 
           (link ([rel "stylesheet"]
                  [type "text/css"]
                  [href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap-theme.min.css"])) 
           (script ([src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"]))
           (script ([src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js"]))
           (link ([rel "stylesheet"] [type "text/css"] [href "/main.css"])))
         (body
           ,(header-template user)
           (div ([class "container"]) ,body))))

(define (paste->xexpr paste)
  `(li ([class "paste-item"])
     (div ([class "paste-row-1"])
       (a ([href ,(get-paste-url (paste-url paste))]) ,(paste-url paste))
       nbsp
       (paste-title ,(paste-title paste)))
     (div ([class "paste-row-2"])
       ,@(if (paste-private? paste)
           (list "["
                 `(a ([href ,(get-paste-source-url (paste-url paste))]) "src")
                 "]")
           '())
       ,@(cond
           [(number? (paste-userid paste)) ; fixme
            (define username (get-username-by-id (paste-userid paste)))
            (list "[" `(a ([href ,(profile-url username)]) ,username) "]")]
           [else '()])
       ,(format "[hits ~a]" (paste-views paste)))))

; response/message : String -> Response
(define-syntax-rule (response/message user msg)
  (response/xexpr
    (page-template "message" user `(body (p ,msg)))))

; do-dispatch : URL -> 
(define-values (do-dispatch url)
  (dispatch-rules
    [("api" "upload") #:method "post" serve-api-upload]
    [("upload") #:method "post" serve-upload]
    [("get" (string-arg)) #:method "get" serve-get]
    [("get-src" (string-arg)) #:method "get" serve-get-src]
    [("auth" "signin") #:method "get" serve-signin]
    [("auth" "signup") #:method "get" serve-signup]
    [("auth" "signoff") #:method "get" serve-signoff]
    [("profile" (string-arg)) #:method "get" serve-profile]
    [("explore") #:method "get" serve-explore]
    [("") serve-default]))

(define (request-session-token req)
  (request-id-cookie SESSION-COOKIE-NAME SESSION-COOKIE-SALT req))

(define (get-session-username req)
  (define token (request-session-token req))
  (and token
       (session-user token)))

(define (logged-in? req)
  (string? (get-session-username req)))

(define (can-access-paste? paste username)
  (cond
    [(or (false? (paste-private? paste)) (equal? (paste-private? paste) 0)) #t]
    [(false? username) #f]
    [else (equal? (get-user-id username) (paste-userid paste))]))

; serve-get : Request Name -> Response
(define (serve-get req name)
  (define paste (get-paste-by-name name))
  (if paste
    (cond
      [(paste-output-ready? paste)
       (paste-views-add1 paste)
       (response/full
         200 #"OK"
         (current-seconds) TEXT/HTML-MIME-TYPE
         '()
         (list (port->bytes (paste-web-output paste))))]
      [else (response/message (get-session-username req)
                              "Paste is not compiled yet! Try again in few seconds")])
    (response/message session-user "Paste not found!")))

; serve-get-src : Request Name -> Response
(define (serve-get-src req name)
  (define paste (get-paste-by-name name))
  (define session-user (get-session-username req))
  (define username (if (number? (paste-userid paste)) ;fixme
                     (get-username-by-id (paste-userid paste))
                     #f))
  (if (and paste (can-access-paste? paste session-user))
    (response/xexpr
      (page-template
        name
        session-user
        `(div ([class "row"])
           (div ([class "col-md-2"])
                (div ([style "font-size: 120%"])
                     (a ([href ,(get-paste-url (paste-url paste))]) ,(paste-url paste)) (br)
                     ,(paste-title paste) (br) (br)
                     "Viewed " ,(number->string (paste-views paste)) " times." (br)
                     ,@(if username
                         (list "Uploaded by " `(a ([href ,(profile-url username)]) ,username))
                         '())))
           (div ([class "col-md-10"])
                (pre (paste-source ,(port->string (paste-source paste))))))))
    (response/message session-user "Paste not found!")))

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
  (define session-user (get-session-username req))
  (define session-userid (and session-user
                              (get-user-id session-user)))
  (cond
    [(exists-binding? 'source bindings)
     (create-paste!
      name
      (string->bytes/utf-8 (extract-binding/single 'source bindings))
      #:private (and (exists-binding? 'private bindings)
                     (extract-binding/single 'private bindings))
      #:title (extract-binding/single 'title bindings)
      #:userid session-userid)
     (response/xexpr
       (page-template
         "upload"
         session-user
         `(div
            (h2 "source uploaded")
            (p ,(string-append
                  "Program successfully uploaded. Compilation can take "
                  "few seconds to complete. ")
               (a ([href ,(get-paste-url name)]) ,(get-paste-url name))))))]
    [else (response/message session-user "Invalid params passed")]))

; serve-default : Request -> Response
(define (serve-default req)
  (define session-user (get-session-username req))
  (response/xexpr
    (page-template
      ""
      session-user
      `(div ([class "row"])
            (div ([class "col-md-9"])
                 (form ([action "/upload"] [method "post"] [id "paste-form"])
                       (input ([type "text"] [name "title"] [class "form-control"] [placeholder "Title"]))
                       (br)
                       (textarea ([cols "80"] [rows "25"] [name "source"] [class "form-control"]))
                       (br)
                       (label (input ([type "checkbox"] [name "private"])) "Private Source?")
                       nbsp nbsp nbsp
                       (input ([type "submit"] [class "btn btn-default"]))))
            (div ([class "col-md-3"])
                 (h4 "recent pastes")
                 (ul ([class "list-unstyled"])
                   ,@(map paste->xexpr (get-recent-pastes 50))))))))

; serve-signup : Request -> Response
(define (serve-signup req)
  (define (get-creds)
    (define req
      (send/suspend
        (λ (k-url)
          (response/xexpr 
            (page-template
              "register"
              #f
              `(div
                 (h2 "register")
                 (div ([style "width: 40%"])
                      (form ([method "post"] [action ,k-url])
                            (div ([class "form-group"])
                                 (label ([for "usernameField"]) "Username")
                                 (input ([type "text"] [class "form-control"] [name "username"] [id "usernameField"] [placeholder "Enter username"]))
                                 (p ([class "help-block"]) "alphabets, digits, _, Length 4-20 characters"))
                            (div ([class "form-group"])
                                 (label ([for "passwordField"]) "Password")
                                 (input ([type "password"] [class "form-control"] [name "password"] [id "passwordField"] [placeholder "Password"]))
                                 (p ([class "help-block"]) "alphabets, digits, _, @, #, $, %, Length 4-20 characters"))
                            (input ([type "submit"] [class "btn btn-default"]))))))))))
    (cons (extract-binding/single 'username (request-bindings req))
          (extract-binding/single 'password (request-bindings req))))

  (response/message
    #f
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
            (page-template
              "signin"
              #f
              `(div
                 (h2 "sign-in")
                 (div ([style "width: 40%"])
                   (form ([method "post"] [action ,k-url])
                         (div ([class "form-group"])
                              (label ([for "usernameField"]) "Username")
                              (input ([type "text"] [class "form-control"] [name "username"] [id "usernameField"] [placeholder "Enter username"])))
                         (div ([class "form-group"])
                              (label ([for "passwordField"]) "Password")
                              (input ([type "password"] [class "form-control"] [name "password"] [id "passwordField"] [placeholder "Password"])))
                         (input ([type "submit"] [class "btn btn-default"]))))))))))
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
       [else (response/message #f "invalid username/password.")])]))

(define (serve-signoff req)
  (destroy-session (request-session-token req))
  (redirect-to "/"
               #:headers
               (map cookie->header
                    (list (logout-id-cookie SESSION-COOKIE-NAME #:path "/")))))

(define (serve-profile req username)
  (define userid (get-user-id username))
  (response/xexpr
    (page-template
      (string-append "profile - " username)
      (get-session-username req)
      `(div
        (h2 "profile: " ,username)
        (div 
          (ul ([class "list-unstyled"])
              ,@(map paste->xexpr (get-user-pastes userid))))))))

(define (serve-explore req)
  (response/xexpr
    (page-template
      (string-append "explore")
      (get-session-username req)
      `(div
         (h2 "explore")
         (div ([class "row"])
              (div ([class "col-md-4"])
                   (h3 "most viewed pastes")
                   (ul ([class "list-unstyled"])
                    ,@(map paste->xexpr (get-most-viewed-pastes 50))))
              (div ([class "col-md-4"])
                   (h3 "recent pastes")
                   (ul ([class "list-unstyled"])
                       ,@(map paste->xexpr (get-recent-pastes 50)))))))))

; get-paste-url : Name -> String
(define (get-paste-url name)
  (string-append SERVER-ROOT "/get/" name))

(define (get-paste-source-url name)
  (string-append SERVER-ROOT "/get-src/" name))

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
                 #:extra-files-paths (list
                                       (string->path STATIC-FILES-DIR))
                 #:launch-browser? false))

; run
(main)
