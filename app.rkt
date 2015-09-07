#lang racket

(require web-server/http
         web-server/http/bindings
         web-server/http/id-cookie
         web-server/servlet/web
         web-server/servlet-env
         web-server/dispatch
         net/uri-codec
         (only-in net/url
                  http-sendrecv/url
                  string->url)
         (prefix-in es: elasticsearch)
         racket/date
         json
         "model.rkt"
         "config.rkt"
         "web.rkt"
         "utils.rkt"
         "search.rkt")

(define MIME-PLAIN-TEXT #"text/plain")

(define STARTER-TEMPLATE-CODE
#<<EOF
#lang whalesong

;; module names are little different from standard racket libs. Just prefix
;; libraries with 'whalesong'
(require whalesong/image)
(require whalesong/world)
(require whalesong/lang/list)

;; ... and now your code

;; Press F11 to toggle fullscreen editor
EOF
)

; TODO: Put constants and messages above

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Helpers

(define (g-recaptcha-verified? request)
  (define bindings (request-bindings request))
  (cond
    [(exists-binding? 'g-recaptcha-response bindings)
     (define response-token
       (extract-binding/single 'g-recaptcha-response bindings)) 
     (define-values
       (status headers captcha-success-in)
       (http-sendrecv/url 
         (string->url "https://www.google.com/recaptcha/api/siteverify")
         #:method "POST"
         #:data (alist->form-urlencoded
                  (list (cons 'secret RECAPTCHA-SECRET-KEY)
                        (cons 'response response-token)
                        (cons 'remoteip (request-client-ip request))))
         #:headers '("Content-Type: application/x-www-form-urlencoded")))
     (hash-ref (read-json captcha-success-in) 'success #f)]
    [else #f]))

; paste -> string
; returns title if available or the paste number for displaying
(define (paste-friendly-title paste)
  (if (zero? (string-length (paste-title paste)))
    (paste-url paste)
    (paste-title paste)))

; -> username?
; returns the username of current request
(define (get-session-username)
  (and (current-session)
       (hash-ref (current-session) 'username)))

; paste username? -> boolean
(define (can-access-paste? paste username)
  (cond
    [(and username (equal? (get-user-id username) (paste-userid paste))) #t]  
    [else (not (paste-private? paste))]))

; paste username -> boolean
(define (can-write-paste? paste username)
  (if (false? username)
    #f
    (equal? (get-user-id username) (paste-userid paste))))

; string -> xexpr?
; xexpr for header which includes navbar
(define (header-template username)
  `(nav ([class "navbar navbar-inverse navbar-fixed-top"])
        (div ([class "container"])
             (div ([class "navbar-header"])
                  (a ([class "navbar-brand"] [href "/"]) "Whalebin"))
             (div ([id "navbar"])
                  (ul ([class "nav navbar-nav"])
                      (li (a ([href "/"]) "New"))
                      (li (a ([href "/explore"]) "Explore")))
                  (ul ([class "nav navbar-nav navbar-right"])
                      (form ([class "navbar-form navbar-left"] [role "search"] [action "/search"])
                            (div ([class "form-group"])
                                 (input ([type "text"] [class "form-control"] [placeholder "Search"] [name "q"]))))
                      ,@(if username
                          (list 
                            `(li (a ([href ,(profile-url username)]) ,username))
                            `(li (a ([href "/auth/signoff"]) "Sign Off")))
                          (list `(li (a ([href "/auth/signin"]) "Sign In"))
                                `(li (a ([href "/auth/signup"]) "Register")))))))))

; string string xexpr? [xexpr?] -> xexpr?
; Returns an xexpr for complete page. body is spliced in betweeen body tags,
; and head-hooks are spliced in head tag
(define (page-template title username body #:head-hooks [head-hooks '()])
  `(html
     (head
       (title ,(string-append title " : whalebin "))
       (meta ([charset "utf-8"]))
       (link ([rel "stylesheet"]
              [type "text/css"]
              [href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap.min.css"])) 
       (link ([rel "stylesheet"]
              [type "text/css"]
              [href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/css/bootstrap-theme.min.css"])) 
       (script ([src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"]))
       (script ([src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.4/js/bootstrap.min.js"]))
       (script ([src "https://www.google.com/recaptcha/api.js"])) ;; not needed everypage, but well...
       (script ([src "/main.js"]))
       (link ([rel "stylesheet"] [type "text/css"] [href "/main.css"]))
       ,@head-hooks)
     (body
       ,(header-template username)
       (div ([class "container"]) ,body))))

; paste -> xexpr?
; web representation for given paste
(define (paste->xexpr paste)
  `(li ([class "paste-item"])
     (div ([class "paste-row-1"])
       (a ([href ,(get-paste-url (paste-url paste))])
          ,(paste-friendly-title paste)))
     (div ([class "paste-row-2"])
       ,@(if (can-access-paste? paste (get-session-username))
           (list "["
                 `(a ([href ,(get-paste-source-url (paste-url paste))]) "src")
                 "]")
           '())
       ,@(cond
           [(number? (paste-userid paste)) ; fixme
            (define username (get-username-by-id (paste-userid paste)))
            (list "[" `(a ([href ,(profile-url username)]) ,username) "]")]
           [else '()])
       ,(format "[hits ~a]" (paste-views paste))
       ,@(if (equal? (paste-compiler-error? paste) 1) ; fixme
           '("[error]")
           '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Handlers


; serve-get-full : Request Name -> Response
(define/session-handler (serve-get-full req name)
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
      [else (response/message (get-session-username)
                              "Paste is not compiled yet! Try again in few seconds")])
    (response/message session-user "Paste not found!")))

; serve-get: Request Name -> Response
(define/session-handler (serve-get req name)
  (define paste (get-paste-by-name name))
  (define session-user (get-session-username))
  (define paste-user (if (number? (paste-userid paste)) ;fixme
                       (get-username-by-id (paste-userid paste))
                       #f))
  (response/xexpr
    (page-template
      (paste-friendly-title paste)
      session-user
      `(div ([class "row"])
            (div ([class "col-md-3"])
                 (p ([id "favorite"] [data-url ,(paste-url paste)]))
                 (div ([style "font-size: 120%"])
                      (p (a ([href ,(get-paste-source-url (paste-url paste))] [class "btn btn-default"]) "Source") nbsp nbsp nbsp
                         (a ([href ,(get-paste-full-url (paste-url paste))] [class "btn btn-default"]) "Fullscreen"))
                      (p (h3 ,(paste-title paste))
                        ,(paste-descp paste))
                      (p ([class "paste-meta"]) 
                        "Paste #" ,(paste-url paste) ,@(if (paste-private? paste)
                                                         (list 'nbsp "(private)")
                                                         (list))
                        (br)
                        ,(number->string (paste-views paste)) " hits" (br)
                        ,@(if paste-user
                            (list "Uploaded by " `(a ([href ,(profile-url paste-user)]) ,paste-user))
                            '())
                        (br)
                        ,@(parameterize ([date-display-format 'iso-8601]) ;;TODO also show time, not just date
                           (list  '(br)
                                  "Updated: " (date->string (paste-last-ts paste))
                                  '(br)
                                  "Created: " (date->string (paste-create-ts paste)))))
                      ,@(if (can-access-paste? paste session-user)
                          (list `(form  ([method "get"] [action "/"])
                                        (input ([type "hidden"]
                                                [name "fork-url"]
                                                [value ,name]))
                                        (input ([type "submit"]
                                                [class "btn btn-default"]
                                                [value "Fork Paste"]))))
                          '())
                      ,@(if (can-write-paste? paste session-user)
                          (list `(form  ([method "get"] [action "/edit"])
                                        (input ([type "hidden"]
                                                [name "url"]
                                                [value ,name]))
                                        (input ([type "submit"]
                                                [class "btn btn-default"]
                                                [value "Edit Paste"]))))
                          `())
                      (p ,@(social-buttons (get-paste-url (paste-url paste))))))
            (div ([class "col-md-9"])
                 ,(if (paste-output-ready? paste)
                    `(iframe [(src ,(get-paste-full-url (paste-url paste)))
                              (class "viewer")
                              (allowfullscreen "true")
                              (width "100%")
                              (height "85%")
                              (frameborder "0")])
                    "Paste is not compiled yet! Try again in few seconds")))
      #:head-hooks (list `(script ([src "/favorite.js"]))))))


; serve-get-src : Request Name -> Response
(define/session-handler (serve-get-src req name)
  (define paste (get-paste-by-name name))
  (define session-user (get-session-username ))
  (define paste-user (if (number? (paste-userid paste)) ;fixme
                     (get-username-by-id (paste-userid paste))
                     #f))
  (if (and paste (can-access-paste? paste session-user))
    (response/xexpr
      (page-template
        (paste-friendly-title paste)
        session-user
        `(div ([class "row"])
           (div ([class "col-md-3"])
                (p ([id "favorite"] [data-url ,(paste-url paste)]))
                (div ([style "font-size: 120%"])
                     (p (a ([href ,(get-paste-url (paste-url paste))] [class "btn btn-default"]) "Execute"))
                     (p (h3 ,(paste-title paste))
                        ,(paste-descp paste))
                     (p ([class "paste-meta"]) 
                        "Paste #" ,(paste-url paste) ,@(if (paste-private? paste)
                                                         (list 'nbsp "(private)")
                                                         (list))
                        (br)
                        ,(number->string (paste-views paste)) " hits" (br)
                        ,@(if paste-user
                            (list "Uploaded by " `(a ([href ,(profile-url paste-user)]) ,paste-user))
                            '())
                        (br)
                        ,@(parameterize ([date-display-format 'iso-8601]) ;;TODO also show time, not just date
                           (list  '(br)
                                  "Updated: " (date->string (paste-last-ts paste))
                                  '(br)
                                  "Created: " (date->string (paste-create-ts paste)))))
                      ,@(if (can-access-paste? paste session-user)
                          (list `(form  ([method "get"] [action "/"])
                                        (input ([type "hidden"]
                                                [name "fork-url"]
                                                [value ,name]))
                                        (input ([type "submit"]
                                                [class "btn btn-default"]
                                                [value "Fork Paste"]))))
                          '())
                     ,@(if (can-write-paste? paste session-user)
                          (list `(form  ([method "get"] [action "/edit"])
                                        (input ([type "hidden"]
                                                [name "url"]
                                                [value ,name]))
                                        (input ([type "submit"]
                                                [class "btn btn-default"]
                                                [value "Edit Paste"]))))
                          `())
                     (p ,@(social-buttons (get-paste-url (paste-url paste))))))
           (div ([class "col-md-9"])
                (pre
                  (code ([class "scheme"])
                    (paste-source ,(port->string (paste-source paste)))))))
        #:head-hooks (list `(script ([src "/highlight.pack.js"]))
                           `(script ([src "/favorite.js"]))
                           `(link ([rel "stylesheet"]
                                   [href "/github.css"]))
                           `(script "hljs.initHighlightingOnLoad();"))))
    (response/message session-user "Paste not found!")))

; serve-edit : Request -> Response
(define/session-handler (serve-edit-save req)
  (define bindings (request-bindings req))
  (define url (extract-binding/single 'url bindings))
  (define title (extract-binding/single 'title bindings))
  (define descp (extract-binding/single 'descp bindings)) 
  (define source (extract-binding/single 'source bindings)) 
  (define private? (and (exists-binding? 'private bindings)
                        (extract-binding/single 'private bindings)))
  (define paste (get-paste-by-name url))
  (define session-user (get-session-username))
  (when (can-write-paste? paste session-user)
    (paste-save!
      (make-paste (paste-id paste)
                  (paste-url paste)
                  title
                  descp
                  (paste-create-ts paste)
                  (current-date)
                  (paste-userid paste)
                  (paste-views paste)
                  (if private? 1 0)
                  (paste-compiler-error? paste))
      source))

  (redirect-to (get-paste-url (paste-url paste))))

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
(define/session-handler (serve-upload req)
  (define name (random-name NAME-LEN))
  (define bindings (request-bindings req))
  (define session-user (get-session-username))
  (define session-userid (and session-user
                              (get-user-id session-user)))
  (cond
    [(and (exists-binding? 'source bindings)
          (g-recaptcha-verified? req))
     (create-paste!
      name
      (string->bytes/utf-8 (extract-binding/single 'source bindings))
      #:private (and (exists-binding? 'private bindings)
                     (extract-binding/single 'private bindings))
      #:title (extract-binding/single 'title bindings)
      #:descp (extract-binding/single 'descp bindings)
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
(define/session-handler (serve-default req)
  (define bindings (request-bindings req))
  (define-values (title descp source)
                 (cond
                   [(exists-binding? 'fork-url bindings)
                    (define paste (get-paste-by-name (extract-binding/single 'fork-url bindings)))
                    (if (can-access-paste? paste session-user)
                      (values (paste-title paste)
                              (paste-descp paste)
                              (port->string (paste-source paste)))
                      (values "" "" STARTER-TEMPLATE-CODE))]
                   [else (values "" "" STARTER-TEMPLATE-CODE)]))
  (response/xexpr
    (get-paste-edit-xexpr "new" title descp source #f "/upload")))

; serve-edit : Request -> Response
(define/session-handler (serve-edit req)
  (define bindings (request-bindings req))
  (define-values (url title descp private? source)
                 (cond
                   [(exists-binding? 'url bindings)
                    (define url (extract-binding/single 'url bindings))
                    (define paste (get-paste-by-name url))
                    (if (can-access-paste? paste (get-session-username))
                      (values url
                              (paste-title paste)
                              (paste-descp paste)
                              (paste-private? paste)
                              (port->string (paste-source paste)))
                      (values #f "" "" #f STARTER-TEMPLATE-CODE))]
                   [else (values #f "" "" #f STARTER-TEMPLATE-CODE)]))
  (response/xexpr
    (get-paste-edit-xexpr (format "edit ~a" url) title descp source private? "/edit-save" url)))

(define (get-paste-edit-xexpr page-title title descp source private? action [paste-url #f])
  (page-template
      page-title
      (get-session-username)
      `(div ([class "row"])
            (div ([class "col-md-9"])
                 (form ([action ,action] [method "post"] [id "paste-form"])
                       (input ([type "text"] [name "title"] [class "form-control"] [placeholder "Title"] [value ,title]))
                       (br)
                       (textarea ([name "descp"] [class "form-control"] [placeholder "Description"] [rows "3"] [style "resize: vertical"]) ,descp)
                       (br)
                       (textarea ([style "font-family: monosapce;"] [cols "80"] [rows "25"] [name "source"] [class "form-control"] [id "source"])
                                 ,source)
                       (br)
                       (div ([class "g-recaptcha"] [data-sitekey ,RECAPTCHA-SITE-KEY]))
                       (br)
                       (label (input ([type "checkbox"] [name "private"] ,@(if private? '([checked ""]) '()))) "Private Source?")
                       nbsp nbsp nbsp
                       ,@(if paste-url
                           (list `(input ([type "hidden"] [name "url"] [value ,paste-url])))
                           `())
                       (input ([type "submit"] [class "btn btn-default"]))))
            (div ([class "col-md-3"])
                 (h4 "recent pastes")
                 (ul ([class "list-unstyled"])
                   ,@(map paste->xexpr (get-recent-pastes 10))))
            
            `(script  #<<EOF
                      var myCodeMirror = CodeMirror.fromTextArea(document.getElementById('source'), {
                        lineNumbers: true,
                        styleActiveLine: true,
                        matchBrackets: true,
                        extraKeys: {
                                "F11": function(cm) {
                                  cm.setOption("fullScreen", !cm.getOption("fullScreen"));
                                  $("nav").toggle(!cm.getOption("fullScreen"));
                                },
                                "Esc": function(cm) {
                                  if (cm.getOption("fullScreen")) cm.setOption("fullScreen", false);
                                  $("nav").toggle(!cm.getOption("fullScreen"));
                                }
                        }
                      });  
EOF
                      ))
      #:head-hooks (list `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.3.0/codemirror.min.js"]))
                         `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.3.0/addon/display/fullscreen.js"]))
                         `(script ([src "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.3.0/mode/scheme/scheme.min.js"]))
                         `(link ([rel "stylesheet"]
                                 [href "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.3.0/codemirror.min.css"]))
                         `(link ([rel "stylesheet"]
                                 [href "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.3.0/addon/display/fullscreen.css"])))))

; serve-signup : Request -> Response
(define/session-handler (serve-signup req)
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

; serve-signin : Request -> Response
(define/session-handler (serve-signin req)
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

; serve-signoff : Request -> Response
(define/session-handler (serve-signoff req)
  (destroy-session (request-session-cookie req))
  (redirect-to "/"
               #:headers
               (map cookie->header
                    (list (logout-id-cookie SESSION-COOKIE-NAME #:path "/")))))

; serve-profile : Request Name -> Response
(define/session-handler (serve-profile req username)
  (define userid (get-user-id username))
  (response/xexpr
    (page-template
      (string-append "profile - " username)
      (get-session-username)
      `(div
         (h2 "profile: " ,username)
         (div ([class "row"])
           (div ([class "col-md-4"])
             (div 
               (ul ([class "list-unstyled"])
                 ,@(map paste->xexpr (get-user-pastes userid)))))
           (div ([class "col-md-4"])
             (div 
               (ul ([class "list-unstyled"])
                 ,@(map paste->xexpr (get-user-starred-pastes userid))))))))))

; serve-explore : Request -> Response
(define/session-handler (serve-explore req)
  (response/xexpr
    (page-template
      (string-append "explore")
      (get-session-username)
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
                       ,@(map paste->xexpr (get-recent-pastes 50))))
              (div ([class "col-md-4"])
                   (h3 "top starred pastes")
                   (ul ([class "list-unstyled"])
                       ,@(map paste->xexpr (get-top-starred-pastes 50)))))))))

(define/session-handler (serve-star-state req url)
  (define session-user (get-session-username))
  (define session-userid (and session-user
                              (get-user-id session-user)))
  (response/jsexpr
    (hash 'starred (and session-userid
                       (paste-starred-by-user?
                         (get-paste-by-name url)
                         session-userid))
          'count (paste-star-counts (get-paste-by-name url)))))

(define/session-handler (serve-star-set req url)
  (define session-user (get-session-username))
  (define session-userid (and session-user
                              (get-user-id session-user)))
  (define paste (get-paste-by-name url))
  (paste-star-by-user paste session-userid)
  (response/jsexpr
    (hash 'starred (and session-userid
                       (paste-starred-by-user?
                         (get-paste-by-name url)
                         session-userid))
          'count (paste-star-counts (get-paste-by-name url)))))

(define/session-handler (serve-star-unset req url)
  (define session-user (get-session-username))
  (define session-userid (and session-user
                              (get-user-id session-user)))
  (define paste (get-paste-by-name url))
  (paste-unstar-by-user paste session-userid)
  (response/jsexpr
    (hash 'starred (and session-userid
                       (paste-starred-by-user?
                         (get-paste-by-name url)
                         session-userid))
          'count (paste-star-counts (get-paste-by-name url)))))

(define/session-handler (serve-search req)
  (define q (extract-binding/single 'q (request-bindings req)))
  (define session-user (get-session-username))
  (define hits
    (hash-refs (search-paste q) 'hits 'hits))
  (response/xexpr
    (page-template
      "search"
      session-user
      `(div ,@(search-hits->xexpr hits)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; URL builder

; profile-user : Name -> String
(define (profile-url username)
  (string-append "/profile/" username))

; get-paste-url : Name -> String
(define (get-paste-url name)
  (string-append SERVER-ROOT "/get/" name))

; get-paste-full-url : Name -> String
(define (get-paste-full-url name)
  (string-append SERVER-ROOT "/get-full/" name))

; get-paste-source-url : Name -> String
(define (get-paste-source-url name)
  (string-append SERVER-ROOT "/get-src/" name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; xexpr snippets

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

(define (social-buttons url)
  twitter-social-button)

(define twitter-social-button
  (list
    `(a ([href "https://twitter.com/share"] [class "twitter-share-button"] [data-hashtags "racketlang"]) "Tweet")
    `(script "!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    [("get-full" (string-arg)) #:method "get" serve-get-full]
    [("get-src" (string-arg)) #:method "get" serve-get-src]
    [("auth" "signin") #:method "get" serve-signin]
    [("auth" "signup") #:method "get" serve-signup]
    [("auth" "signoff") #:method "get" serve-signoff]
    [("ajax" "star" "state" (string-arg)) #:method "get" serve-star-state]
    [("ajax" "star" "set" (string-arg)) #:method "get" serve-star-set]
    [("ajax" "star" "unset" (string-arg)) #:method "get" serve-star-unset]
    [("profile" (string-arg)) #:method "get" serve-profile]
    [("explore") #:method "get" serve-explore]
    [("edit") #:method "get" serve-edit]
    [("edit-save") #:method "post" serve-edit-save]
    [("search") #:method "get" serve-search]
    [("") serve-default]))

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
