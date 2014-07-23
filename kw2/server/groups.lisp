(in-package :kw2)

(defun groups-handler (uri)
 (if *session*
  uri
  (redirect "/")))

