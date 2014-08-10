(in-package :kw2)

(defvar *db-connection-parameters* (list "kw2" "kw2" "kw2" "localhost" :pooled-p t))

(defun create-schema ()
 (with-connection *db-connection-parameters*
  (unless (table-exists-p 'users)
   (execute
    (:create-table users
     ((pk_id :type serial :primary-key t)
      (email :type string)
      (password :type string)
      (display_name :type (or db-null string))))))

  (unless (table-exists-p 'groups)
   (execute
    (:create-table groups
     ((pk_id :type serial :primary-key t)
      (alias :type text)
      (name :type text)
      (admin :type integer)
      (flags :type integer :default 0)))))

  (unless (table-exists-p 'acl)
   (execute
    (:create-table acl
     ((fk_user_id :type integer)
      (fk_group_id :type integer)
      (flags :type integer :default 0))))
   (execute
    (:create-index 'acl_by_user :on "acl" :fields 'fk_user_id))
   (execute
    (:create-index 'acl_by_group :on "acl" :fields 'fk_group_id)))

  (unless (table-exists-p 'posts)
   (execute
    (:create-table posts
     ((pk_id :type serial :primary-key t)
      (fk_parent_post_id :type (or db-null integer))
      (fk_group_id :type integer)
      (fk_user_id :type integer)
      (message_id :type text)
      (subject :type text)
      (headers :type text)
      (post_date :type timestamp)
      (body :type text))))
   (execute
    (:create-index 'posts_by_group :on "posts" :fields 'fk_group_id))
   (execute
    (:create-index 'posts_by_message_id :on "posts" :fields 'message_id)))
 )
)
