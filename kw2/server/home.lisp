(in-package :kw2)

(defun home-handler ()
 (if *session*
  "<h1>WELCOME HOME!</h1>"
  (redirect "/")))


