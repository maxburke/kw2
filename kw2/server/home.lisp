(in-package :kw2)

(defun home-handler ()
 (if *session*
  (handle-static-file #p"static/home.html")
  (redirect "/")))

