(in-package :kw2)

(defun mail-handle-post ()
 (let* ((content (post-parameter "data")))
 )
)

(defun mail-handler ()
 (let ((req (request-method* *request*)))
  (if (eq req :post)
   (mail-handle-post)
   (progn
    (setf (return-code*) +http-method-not-allowed+)
    "Method not allowed!"))))
