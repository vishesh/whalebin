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
  `(div ([id "menu-container"])
        (ul ([id "menu"])
            (li (a ([href "#"]) "Whalebin"))
            (li (a ([href "/"]) "New"))
            (li (a ([href "/explore"]) "Explore"))
            ,@(if user
               (list 
                 `(li (a ([href ,(profile-url user)]) ,user))
                 `(li (a ([href "/auth/signoff"]) "Sign Off")))
               (list `(li (a ([href "/auth/signin"]) "Sign In"))
                     `(li (a ([href "/auth/signup"]) "Register")))))))

(define (page-template title user body)
  `(html (head
           (title ,(string-append "whalebin " title))
           (meta ([charset "utf-8"]))
           (link ([rel "stylesheet"] [type "text/css"] [href "/main.css"])))
         (body
           ,(header-template user)
           ,body)))

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
    [("auth" "signin") #:method "get" serve-signin]
    [("auth" "signup") #:method "get" serve-signup]
    [("auth" "signoff") #:method "get" serve-signoff]
    [("profile" (string-arg)) #:method "get" serve-profile]
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
  (define session-user (get-session-username req))
  (if (and paste (can-access-paste? paste session-user))
    (cond
      [(paste-output-ready? paste)
       (paste-views-add1 paste)
       (response/full
         200 #"OK"
         (current-seconds) TEXT/HTML-MIME-TYPE
         '()
         (list (port->bytes (paste-web-output paste))))]
      [else (response/message session-user
                              "Paste is not compiled yet! Try again in few seconds")])
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
  (define (paste->li p)
    `(li (a ([href ,(get-paste-url (paste-url p))]) ,(paste-url p))))
  (response/xexpr
    (page-template
      ""
      session-user
      `(div ([id "page"])
            (div ([id "paste-column"])
                 (form ([action "/upload"] [method "post"] [id "paste-form"])
                       (textarea ([cols "100"] [rows "30"] [name "source"]))
                       (br) (br)
                       (input ([type "checkbox"] [name "private"]))
                       (label "Private Paste")
                       (br) (br)
                       (input ([type "checkbox"] [name "publish"]))
                       (label "Private Source")
                       (br) (br)
                       (input ([type "submit"] [class "submit-button"]))))
            (div ([id "recent-pastes-column"])
                 (h4 "recent pastes")
                 (ul
                   ,@(map paste->li (get-recent-pastes))))))))

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
                 (h3 "register")
                 (div
                   (form ([method "post"] [action ,k-url])
                         (label "Username ")
                         (input ([type "text"] [name "username"])) (br)
                         (label "Password ")
                         (input ([type "password"] [name "password"]))
                         (br) (br)
                         (input ([type "submit"]))))))))))
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
                 (h3 "sign-in")
                 (div
                   (form ([method "post"] [action ,k-url])
                         (label "Username ")
                         (input ([type "text"] [name "username"])) (br)
                         (label "Password ")
                         (input ([type "password"] [name "password"]))
                         (br) (br)
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
       [else (response/message #f "invalid username/password.")])]))

(define (serve-signoff req)
  (destroy-session (request-session-token req))
  (redirect-to "/"
               #:headers
               (map cookie->header
                    (list (logout-id-cookie SESSION-COOKIE-NAME #:path "/")))))

(define (serve-profile req username)
  (define userid (get-user-id username))
  (define (paste->li p)
    `(li (a ([href ,(get-paste-url (paste-url p))]) ,(paste-url p))))
  (response/xexpr
    (page-template
      (string-append "profile - " username)
      (get-session-username req)
      `(div
        (h5 "profile: " ,username
            (div 
              (ul ,@(map paste->li (get-user-pastes userid)))))))))

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
                 #:extra-files-paths (list
                                       (string->path STATIC-FILES-DIR))
                 #:launch-browser? false))

; run
(main)
