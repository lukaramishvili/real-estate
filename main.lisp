;;; load dependencies
(push :hunchentoot-no-ssl *features*)
(ql:quickload 'hunchentoot)
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

(defvar *project-load-path* "/projects/re/")
;;(defvar *project-load-path* "D:/htdocs/lisp/re/")

(defun project-load (file-path)
  (load (concatenate 'string *project-load-path* file-path)))

(project-load "utils.lisp")
(project-load "orm.lisp")
(project-load "templates.lisp")
(project-load "style.lisp")

;;;start server
(defvar *htoot*
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor :port 4242)))

(defun default-lang () "geo")

(defun re-tr (keyword &key lang)
  (tr keyword (or lang (session-value 'lang) (default-lang))))


(defmacro htoot-handler ((page-name uri params) &body body)
  `(hunchentoot:define-easy-handler (,page-name :uri ,uri) (,@params)
     (setf (hunchentoot:content-type*) "text/html")
     ,@body))

(htoot-handler (home "/" ())
	       (re-main :title "Browse Real Estate in Real Time!"
			:body (re-firstpage)))

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