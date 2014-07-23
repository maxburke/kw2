(in-package :kw2)

(defun posts-handler (uri)
 (if *session*
  (redirect "/")))

