(defpackage #:kw2-system
 (:use :cl :asdf))

(in-package :asdf)

(defsystem "kw2"
 :serial t
 :depends-on (:hunchentoot :cl-who :postmodern :st-json :uuid :ironclad :cl-smtp :cl-base64)
 :components (
   (:module :server
    :serial t
    :components (
       (:static-file "kw2.asd")
       (:file "package")
       (:file "parameters")
       (:file "init")
       (:file "index")
       (:file "home")
       (:file "join")
       (:file "html-helpers")
       (:file "bad-passwords")
       (:file "groups")
       (:file "posts")
       (:file "session")
       (:file "mail")
       (:file "create-schema")
       (:file "kw2_key")
      )
    )
  )
)


