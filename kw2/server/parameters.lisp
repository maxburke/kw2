(in-package :kw2)

(defvar *server-instance* nil)
(defvar *kw2-index* #p"/home/max/src/lisp/kw2/kw2/static/index.html")
(defvar *db-connection-parameters* (list "kw2" "kw2" "kw2" "localhost" :pooled-p t))

;; Encode the user agent into the session secret so that a new session is required if this changes.
(setf *use-user-agent-for-sessions* t)
;; Encode the remote address into the session secret so that a new session is required if this changes.
(setf *use-remote-addr-for-sessions* t)
;; Set the session max time to be 7 days.
(setf *session-max-time* 604800)
; Permit post-parameters to be retrieved from PUT and DELETE methods, as well as POST
(nconc *methods-for-post-parameters* '(:put :delete))

; These three should be *off* for release.
(setf *show-lisp-errors-p* t)
(defvar *message-log-pathname* #p"/home/max/src/lisp/kw2/logs/message.log")
(defvar *access-log-pathname* #p"/home/max/src/lisp/kw2/logs/access.log")
(defvar *kw2-debug* t)

(if *kw2-debug*
 (defmacro server-log (str)
  `(log-message *lisp-warnings-log-level* ,str))
 (defmacro server-log (str)))

; Debugging facilities
(defvar *kw2-debug* t)

(if *kw2-debug*
 (defmacro server-log (str)
  `(log-message *lisp-warnings-log-level* ,str))
 (defmacro server-log (str)))

; *successful-post-response* is a generic JSON response for when something is returned to
; a client. It is equivalent to { "success" : "true" }. "true"/"false" is currently used,
; as opposed to the boolean true/false values, because cl-json cannot serialize false, 
; only null.
(defvar *successful-post-response* (st-json:write-json-to-string (st-json:jso "success" :true)))
(defvar *unsuccessful-post-response* (st-json:write-json-to-string (st-json:jso "success" :false)))

