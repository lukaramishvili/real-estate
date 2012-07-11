;;; load dependencies
(push :hunchentoot-no-ssl *features*)
(ql:quickload :hunchentoot)
(ql:quickload :cl-who)
(ql:quickload :parenscript)
(ql:quickload :ironclad)
(ql:quickload :uuid)
(ql:quickload :postmodern)
(ql:quickload :css-lite)
(ql:quickload :cl-fad)

;;; declare package
(defpackage :re
  (:use :common-lisp :cl-user :hunchentoot 
	:postmodern :cl-who :ps))
;; :ironclad :css-lite ))

(in-package :re)

(defparameter *project-load-path* "/projects/re/")
#+:WINDOWS-TARGET
(defparameter *project-load-path* "D:/htdocs/lisp/re/")

(defparameter *project-tmp-dir* "/re-tmp/")
#+:WINDOWS-TARGET
(defparameter *project-tmp-dir* "D:/re-tmp/")

(defparameter *upload-dir* "/re-uploads/")
#+:WINDOWS-TARGET
(defparameter *upload-dir* "D:/re-uploads/")

(defun project-load (file-path)
  (load (concatenate 'string *project-load-path* file-path)))

(project-load "utils.lisp")
(project-load "orm.lisp")
(project-load "templates.lisp")
(project-load "style.lisp")
(project-load "js.lisp")

;;;start server
(defvar *htoot*
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor :port 4343)))

(defun default-lang () "geo")

(defun re-tr (keyword &key lang)
  (tr keyword (or lang (session-value 'lang) (default-lang))))


(defmacro htoot-handler ((page-name uri params) &body body)
  `(hunchentoot:define-easy-handler (,page-name :uri ,uri) (,@params)
     (setf (hunchentoot:content-type*) "text/html")
     ,@body))


;;serve css folder
(push (create-folder-dispatcher-and-handler
       "/css/" (pathname (+s *project-load-path* "css/")))
      hunchentoot:*dispatch-table*)

;;serve js folder
(push (create-folder-dispatcher-and-handler
       "/js/" (pathname (+s *project-load-path* "js/")))
      hunchentoot:*dispatch-table*)

;;serve temp folder
(push (create-folder-dispatcher-and-handler
       "/tmp/" (pathname *project-tmp-dir*))
      hunchentoot:*dispatch-table*)

;;serve temp folder
(push (create-folder-dispatcher-and-handler
       "/uploads/" (pathname *upload-dir*))
      hunchentoot:*dispatch-table*)

(defun linkable-tmp-path (tmp-path)
  (+s "/tmp/" (file-namestring tmp-path)))

(defun linkable-pic-path (p)
  (declare (type pic p))
  (smake "/uploads/pics/" (ix-pic p) "/" (file-namestring (path p))))
      

(htoot-handler (home "/" ())
	       (re-main :title "Browse Real Estate in Real Time!"
			:body (re-firstpage)))

(htoot-handler (re-css-handler "/re-gen.css" ())
  (setf (hunchentoot:content-type*) "text/css")
  (re-gen-css))

(htoot-handler (re-js-handler "/main.js" ())
  (setf (hunchentoot:content-type*) "text/css")
  (re-main-js))

(htoot-handler
 (account-page-handler "/account" ())
 (re-main 
  :title (re-tr :account-settings)
  :body   (if (session-value 'logged-in-p) 
	      (account-page (ix-user (session-value 'user-authed)))
	    (+s (re-tr :not-logged-in-please-log-in)
		(login-page :redir "/account")))))

(htoot-handler
 (log-in-handler
  "/login-handler"
  ((redir :request-type :POST :parameter-type 'string :init-form "/")
   (usr :request-type :POST :parameter-type 'string)
   (pwd :request-type :POST :parameter-type 'string)))
  (re-main 
   :title (re-tr :login-page-title)
   :body 
   (if 
    (session-value 'logged-in-p)
    (redirect redir);(re-tr :already-logged-in)
    (let ((user-authed (user-if-valid usr pwd)))
      (if user-authed 
	  (progn
	    (start-session)
	    (setf (session-value 'user-authed) user-authed)
	    (setf (session-value 'logged-in-p) t)
	    (redirect redir));"successfully logged in")
	  (re-tr :couldnt-log-in))))))

(htoot-handler
 (register-page-handler
  "/register" ())
 (re-main
  :title (re-tr :register-page-title)
  :body
  (if (session-value 'logged-in-p)
      (re-tr :already-logged-in)
    (let ((reg-token (+s (uuid:make-v4-uuid))))
      (progn
	(setf (session-value 'reg-token) reg-token)
	(register-page :reg-token reg-token))))))

(htoot-handler
 (register-handler
  "/register-handler"
  ((usr :request-type :POST :parameter-type 'string :init-form nil)
   (pwd :request-type :POST :parameter-type 'string :init-form nil)
   (reg-token :request-type :POST :parameter-type 'string :init-form nil)))
 (if (session-value 'logged-in-p)
     (re-tr :already-logged-in)
   (if (and usr pwd reg-token
	    (plusp (length usr)) (plusp (length pwd))
	    (string= (session-value 'reg-token) reg-token))
       (let ((usr-to-save
	      (make-instance 'user
			     :login-name usr
			     :passwd (hash-password pwd))))
	 (if (plusp (save-user usr-to-save))
	     (re-tr :registration-successful)
	     (re-tr :couldnt-register-correct-errors)))
       (re-tr :couldnt-save-user))))
					 

(htoot-handler
    (log-out-handler "/logout" ())
  (re-main 
   :title (re-tr :logout-page-title)
   :body (if (session-value 'logged-in-p)
	     (progn
	       (setf (session-value 'logged-in-p) nil)
	       (remove-session *session*)
	       (redirect "/"));"you are now logged out")
	     (redirect "/"))));"you are not logged in")))

(htoot-handler
    (login-page-handler
     "/login"
     ((redir :request-type :GET :parameter-type 'string :init-form "/")))
  (re-main 
   :title (re-tr :login-page-title)
   :body (if (session-value 'logged-in-p)
	     (re-firstpage)
	     (login-page :redir redir))))

;;; project-specific code

(htoot-handler
    (estate-edit-handler
     "/edit-estate"
     ((ix-estate :request-type :GET :parameter-type 'integer 
		 :init-form 0)))
  (if (session-value 'logged-in-p)
      (let ((ed-estate (or (with-re-db (get-dao 'estate ix-estate))
			   (make-instance 'estate))))
	;;prepare space for temporary variables
	(if (not (session-value 'rem-pics))
	    (setf (session-value 'rem-pics) (make-hash-table :test 'equal)))
	(re-main :title "Edit real estate"
		 :body (estate-edit-form ed-estate)))
      (login-page :redir "/edit-estate")))

(htoot-handler
    (estate-save-handler
     "/save-estate"
     ((ix-estate :request-type :POST :parameter-type 'integer :init-form 0)
      (address :request-type :POST :parameter-type 'string :init-form "")
      (telnum :request-type :POST :parameter-type 'string :init-form "")
      (ix-main-pic :request-type :POST :parameter-type 'integer :init-form 0)
      (loc-lat :request-type :POST :parameter-type 'string :init-form "0")
      (loc-lng :request-type :POST :parameter-type 'string :init-form "0")
      (visible :request-type :POST :parameter-type 'integer :init-form 0)))
  (if (session-value 'logged-in-p)
      (let* ((save-e
	      (make-instance
	       'estate :ix-user (ix-user (session-value 'user-authed))
	       :address address :telnum telnum :visible visible
	       :ix-main-pic ix-main-pic :loc-lat loc-lat :loc-lng loc-lng))
	     (e-pics
	      (loop for k being the hash-keys of (session-value 'rem-pics)
		 using (hash-value v)
		 collecting v)))
	(progn
	  (if (> ix-estate 0) (setf (ix-estate save-e) ix-estate))
	  (with-re-db 
	    (if 
	     (save-dao save-e)
	     (progn
	       (loop for e-p in e-pics
		  do (setf (ix-estate e-p) (ix-estate save-e))
		    (save-dao e-p);save to get insert-id
		    (ensure-directories-exist 
		     (smake *upload-dir* "pics/" (ix-pic e-p) "/"))
		    (setf (path e-p) 
			  (+s (rename-file 
			       (path e-p)
			       (smake *upload-dir* "pics/" (ix-pic e-p) "/" 
				      (file-namestring (path e-p))))))
		    (save-dao e-p));now save to update path
	       "Real estate saved!")
	     "Error while saving estate!"))))
      "Not logged in!"))

(htoot-handler (estate-form-pic-box-handler "/estate-form-pic-box" 
					    ((rem-pic-uuid :init-form "")))
  (estate-form-pic-box rem-pic-uuid))

(htoot-handler
    (remember-pic
     "/rem-pic"
     ((rem-pic-uuid :parameter-type 'string :init-form "")
      (ix-pic :parameter-type 'integer :init-form 0)
      (ix-estate :parameter-type 'integer :init-form 0)
      (order :parameter-type 'integer :init-form 0)))
  (let ((uploaded-img (post-parameter "img")))
    (if uploaded-img
	(destructuring-bind (path file-name content-type)
	    uploaded-img
	  (let* ((uniq-rem-pic-uuid (if (plusp (length rem-pic-uuid))
				       rem-pic-uuid 
				       (+s (uuid:make-v4-uuid))))
		 (path-tmp (+s *project-tmp-dir* uniq-rem-pic-uuid ".jpg"))
		 (path-tmp-linkable (linkable-tmp-path path-tmp))
		 (pic-to-rem
		  (make-instance 
		   'pic :path path-tmp :order order :ix-estate ix-estate)))
	    (if (> 0 ix-pic) (setf (ix-pic pic-to-rem) ix-pic))
	    (cl-fad:copy-file path path-tmp :overwrite t)
	    (setf (gethash uniq-rem-pic-uuid (session-value 'rem-pics)) 
		  pic-to-rem)
	    (estate-form-pic-box uniq-rem-pic-uuid))))))