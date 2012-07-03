;;; load dependencies
(push :hunchentoot-no-ssl *features*)
(ql:quickload :hunchentoot)
(ql:quickload :cl-who)
(ql:quickload :parenscript)
(ql:quickload :ironclad)
(ql:quickload :uuid)
(ql:quickload :postmodern)
(ql:quickload :css-lite)

;;; declare package
(defpackage :re
  (:use :common-lisp :cl-user :hunchentoot 
	:postmodern :cl-who :ps))
;; :ironclad :css-lite ))

(in-package :re)

(defparameter *project-load-path* "/projects/re/")
#+:WINDOWS-TARGET
(defparameter *project-load-path* "D:/htdocs/lisp/re/")

(defun project-load (file-path)
  (load (concatenate 'string *project-load-path* file-path)))

(project-load "utils.lisp")
(project-load "orm.lisp")
(project-load "templates.lisp")
(project-load "style.lisp")

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
      

(htoot-handler (home "/" ())
	       (re-main :title "Browse Real Estate in Real Time!"
			:body (re-firstpage)))

(htoot-handler (re-css-handler "/re-gen.css" ())
  (setf (hunchentoot:content-type*) "text/css")
  (re-gen-css))


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
    (re-tr :already-logged-in)
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
  (let ((ed-estate (or (with-re-db (get-dao 'estate ix-estate))
		       (make-instance 'estate))))
    (re-main :title "Edit real estate"
	     :body (estate-edit-form ed-estate))))

(htoot-handler
    (estate-save-handler
     "/save-estate"
     ((ix-estate :request-type :POST :parameter-type 'integer :init-form 0)
      (address :request-type :POST :parameter-type 'string :init-form "")
      (telnum :request-type :POST :parameter-type 'string :init-form "")
      (ix-main-pic :request-type :POST :parameter-type 'string :init-form 0)
      (visible :request-type :POST :parameter-type 'integer :init-form 0)))
  (if (session-value 'logged-in-p)
      (let ((save-e
	     (make-instance
	      'estate :ix-user (ix-user (session-value 'user-authed))
	      :address address :telnum telnum :visible visible
	      :ix-main-pic ix-main-pic)))
	(progn
	  (if (> ix-estate 0) (setf (ix-estate save-e) ix-estate))
	  (with-re-db (if (save-dao save-e)
			  "Real estate saved!"
			  "Error while saving estate!"))))
      "Not logged in!"))

(htoot-handler (estate-form-pic-box-handler "/estate-form-pic-box" ())
  (estate-form-pic-box))

(htoot-handler
    (remember-pic
     "/rem-pic"
     ((ix-pic :parameter-type 'integer :init-form 0)
      (ix-estate :parameter-type 'integer :init-form 0)
      (order :parameter-type 'integer)))
  (let ((uploaded-img (post-parameter "img")))
    (if uploaded-img
	(destructuring-bind (path file-name content-type)
	    uploaded-img
	  (let ((pic-to-rem
		 (make-instance 
		  'pic :path path :order order :ix-estate ix-estate)))
	    (if (> 0 ix-pic) (setf (ix-pic pic-to-rem) ix-pic))
	    (push pic-to-rem
		  (session-value rem-pics))
	    "")))))