;;;; Contains helpers for handling html page generation (like generating headers and footers)

(in-package :kw2)

(defmacro with-posted-json ((json) &body body)
 (let ((raw-json-string (gensym)))
  `(let* ((,raw-json-string (octets-to-string (raw-post-data :request *request*) :external-format :utf8))
          (,json (handler-case (json:decode-json-from-string ,raw-json-string)
                      (end-of-file () (server-log (format nil "unable to parse json request ~a" ,raw-json-string))
                                      nil))))
    ,@body
   )
 )
)

; Creates a SHA1 digest, digest, from the given byte vector buffer and executes
; the provided body.
(defmacro with-sha1-digest ((digest buffer) &body body)
 (let ((digester-var (gensym)))
  `(let ((,digester-var (ironclad:make-digest :sha1)))
    (ironclad:update-digest ,digester-var ,buffer)
    (let ((,digest (ironclad:produce-digest ,digester-var)))
     ,@body
    )
   )
 )
)


