(in-package :kw2)

(defvar *db-connection-parameters* (list "kw2" "kw2" "kw2" "localhost" :pooled-p t))

(defun create-schema ()
 (with-connection *db-connection-parameters*
  (unless (table-exists-p 'users)
   (execute
    (:create-table users
     ((user_id :type serial :primary-key t)
      (email :type text)
      (password :type text)
      (display-name :type text)))))

  (unless (table-exists-p 'groups)
   (execute
    (:create-table groups
     ((group_id :type serial :primary-key t)
      (name :type text)
      (admin :type integer)
      (flags :type integer :default 0)))))

  (unless (table-exists-p 'acl)
   (execute
    (:create-table acl
     ((user_id :type integer)
      (group_id :type integer)
      (flags :type integer :default 0))))
   (execute
    (:create-index 'acl_by_user :on "acl" :fields 'user_id))
   (execute
    (:create-index 'acl_by_group :on "acl" :fields 'group_id)))

  (unless (table-exists-p 'posts)
   (execute
    (:create-table posts
     ((post_id :type serial :primary-key t)
      (parent_id :type integer)
      (group_id :type integer)
      (message_id :type text)
      (subject :type text)
      (headers :type text)
      (body :type text))))
   (execute
    (:create-index 'posts_by_group :on "posts" :fields 'group_id)
    (:create-index 'posts_by_message_id :on "posts" :fields 'message_id))
 )
)
