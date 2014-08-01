(in-package :kw2)

(defmacro variable-dispatcher (content-type content)
  `(lambda ()
    (setf (content-type*) ,content-type)
    ,content))

(defun split-uri-by-slashes (uri)
 (let ((collection '()))
  (labels ((recursive-extract (uri-fragment)
            (if uri-fragment
             (let ((fragment-end (position #\/ uri-fragment)))
              (push (subseq uri-fragment 0 fragment-end) collection)
              (if fragment-end
               (recursive-extract (subseq uri-fragment (1+ fragment-end))))))))
  (recursive-extract (subseq uri (1+ (position #\/ uri)))))
  (nreverse collection)
 )
)

(defmacro uri-dispatcher (fn)
 `(lambda ()
     (let ((uri-components (split-uri-by-slashes (request-uri* *request*))))
      (funcall ,fn uri-components))
  )
)
    
(defun init ()
 (create-schema)

 (if (not (null *server-instance*))
  (stop *server-instance*))

 (setf *server-instance* 
  (make-instance 'easy-acceptor :port 8000))

 (start *server-instance*)

 (setf *dispatch-table*
  (nconc (list 'dispatch-easy-handlers
          (create-folder-dispatcher-and-handler "/static/" #p"static/")
          (create-prefix-dispatcher "/api/groupvalidate" (uri-dispatcher #'new-group-validation-handler))
          (create-prefix-dispatcher "/api/groups" (uri-dispatcher #'groups-handler))
          (create-prefix-dispatcher "/mail" #'mail-handler)
          (create-prefix-dispatcher "/groups/new" #'new-group-handler)
          (create-prefix-dispatcher "/groups" #'home-handler)
          (create-prefix-dispatcher "/home" #'home-handler)
          (create-prefix-dispatcher "/join" #'join-handler)
          (create-prefix-dispatcher "/sessions" #'sessions-handler)
          (create-prefix-dispatcher "/index" #'index-handler)
          (create-prefix-dispatcher "/" #'index-handler)
          'default-dispatcher))))

