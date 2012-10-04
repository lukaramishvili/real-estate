;;; load dependencies
(push :hunchentoot-no-ssl *features*)
(ql:quickload :hunchentoot)
(ql:quickload :cl-who)
(ql:quickload :parenscript)
(ql:quickload :cl-json)
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

(defun default-lang () "nl")

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

(defun broker-logo-url (ix-user)
  (if (cl-fad:file-exists-p (smake *upload-dir* "users/" ix-user "/logo.png"))
      (smake "/uploads/users/" ix-user "/logo.png")
      ""));"/css/img/no-pic.jpg" is inappropriate for brokers without logo

(htoot-handler (home "/" ())
	       (re-main :title "Browse Real Estate in Real Time!"
			:body (re-firstpage)))

(htoot-handler (re-css-handler "/re-gen.css" ())
  (setf (hunchentoot:content-type*) "text/css")
  (re-gen-css))

(htoot-handler (re-js-handler "/main.js" ())
  (setf (hunchentoot:content-type*) "text/javascript")
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
  "/register" ((type :parameter-type 'string :init-form "simple")))
 (re-main
  :title (re-tr :register-page-title)
  :body
  (if (session-value 'logged-in-p)
      (re-tr :already-logged-in)
    (let ((reg-token (+s (uuid:make-v4-uuid))))
      (progn
	(setf (session-value 'reg-token) reg-token)
	(register-page :reg-token reg-token :acc-type type))))))

(htoot-handler
    (register-handler
     "/register-handler"
     ((usr :request-type :POST :parameter-type 'string :init-form nil)
      (pwd :request-type :POST :parameter-type 'string :init-form nil)
      (reg-token :request-type :POST :parameter-type 'string :init-form nil)
      (acc-type :request-type :POST :parameter-type 'string :init-form "simple")
      (email :request-type :POST :parameter-type 'string :init-form "")
      (fname :request-type :POST :parameter-type 'string :init-form "")
      (lname :request-type :POST :parameter-type 'string :init-form "")
      (url :request-type :POST :parameter-type 'string :init-form "")
      (telnum :request-type :POST :parameter-type 'string :init-form "")))
  (if (session-value 'logged-in-p)
      (re-tr :already-logged-in)
      (if (and usr pwd ;reg-token
	       (plusp (length usr)) (plusp (length pwd))
	       ;;(string= (session-value 'reg-token) reg-token)
	       )
	  (let* ((checked-type (if (valid-acc-type-p acc-type)
				   acc-type "simple"))
		 (usr-to-save
		  (make-instance 'user
				 :username usr :passwd (hash-password pwd)
				 :acc-type checked-type :email email 
				 :fname fname :lname lname :url url
				 :telnum telnum)))
	    (if (plusp (save-user usr-to-save))
		(progn
		  (let* ((uploaded-logo (post-parameter "logo"))
			 (logo-dest-dir (smake *upload-dir* "users/" (ix-user usr-to-save)))
			 (logo-dest (smake logo-dest-dir "/logo.png")))
		    (if (and uploaded-logo (listp uploaded-logo))
			(destructuring-bind (path file-name content-type)
			    uploaded-logo
			  (cl-fad::ensure-directories-exist logo-dest-dir)
			  (cl-fad:copy-file path logo-dest :overwrite t))))
		  (redirect "/#register-success"))
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
	;;if editing an estate for the first time or switching to another,
	;;then prepare space for temporary variables
	(when (or (/= ix-estate 
		      (or (session-value 'ix-editing-estate) 0))
		  (not (session-value 'rem-pics)))
	  (setf (session-value 'rem-pics) (make-hash-table :test 'equal))
	  ;;when editing an existing estate, and 'rem-pics isn't populated
	  ;;yet, then fill temp with existing pics
	  (when (< 0 ix-estate)
	    (loop for p in (estate-pics ix-estate)
	       do 
		 (let* ((rand-uuid (+s (uuid:make-v4-uuid)))
			(temp-loc (+s *project-tmp-dir* rand-uuid ".jpg")))
		   ;;copy each pic to temp dir for editing
		   (cl-fad:copy-file (path p) temp-loc :overwrite t)
		   ;;pic-s in 'rem-pics are expected to have path in temp dir
		   (setf (path p) temp-loc)
		   (setf (gethash rand-uuid (session-value 'rem-pics)) p)
		   ))))
	;;save in session, which estate is being edited (0 means not saved yet)
	(setf (session-value 'ix-editing-estate) ix-estate)
	(re-main :title "Edit real estate"
		 :body (estate-edit-form ed-estate)))
      (login-page :redir "/edit-estate")))

;;;returns a list of success (bool), ix-estate, error message
(defun save-estate-and-pics (ix-estate save-e e-pics &key main-pic-uuid)
  (if (> ix-estate 0) (setf (ix-estate save-e) ix-estate))
  (with-re-db 
    (if 
     (if (> ix-estate 0) 
	 (update-dao save-e)
	 (insert-dao save-e))
     (progn
       (loop for e-p-k being the hash-keys of e-pics using (hash-value e-p)
	  do (setf (ix-estate e-p) (ix-estate save-e))
	    (save-dao e-p);save to get insert-id
	    (ensure-directories-exist 
	     (smake *upload-dir* "pics/" (ix-pic e-p) "/"))
	    (setf (path e-p) 
		  (+s (rename-file 
		       (path e-p)
		       (smake *upload-dir* "pics/" (ix-pic e-p) "/" 
			      (file-namestring (path e-p))))))
	    (save-dao e-p);now save to update path
	    (when (and main-pic-uuid (plusp (length main-pic-uuid)) 
		       (string= e-p-k main-pic-uuid)) 
		(setf (ix-main-pic save-e) (ix-pic e-p))
		;;here save-e is definitely in db
		(update-dao save-e))
	    )
       (list t (ix-estate save-e) "Real estate saved!"))
     (list nil ix-estate "Error while saving estate!"))))

(htoot-handler
    (estate-save-handler
     "/save-estate"
     ((ix-estate :request-type :POST :parameter-type 'integer :init-form 0)
      (address :request-type :POST :parameter-type 'string :init-form "")
      (telnum :request-type :POST :parameter-type 'string :init-form "")
      (ix-main-pic :request-type :POST :parameter-type 'integer :init-form 0)
      (main-pic-uuid :request-type :POST :parameter-type 'string :init-form "")
      (loc-lat :request-type :POST :parameter-type 'string :init-form "0")
      (loc-lng :request-type :POST :parameter-type 'string :init-form "0")
      (visible :request-type :POST :parameter-type 'integer :init-form 0)
      
      (apt-type :request-type :POST :parameter-type 'string :init-form "")
      (status :request-type :POST :parameter-type 'string :init-form "")
      (pst-code :request-type :POST :parameter-type 'string :init-form "")
      (munic :request-type :POST :parameter-type 'string :init-form "")
      (ix-country :request-type :POST :parameter-type 'integer :init-form 0)
      (constr :request-type :POST :parameter-type 'string :init-form "")
      (total :request-type :POST :parameter-type 'integer :init-form 0)
      (land :request-type :POST :parameter-type 'integer :init-form 0)
      (desc :request-type :POST :parameter-type 'string :init-form "")
      (zmh :request-type :POST :parameter-type 'string :init-form "")
      (price :request-type :POST :parameter-type 'string :init-form 0)
      (since :request-type :POST :parameter-type 'integer :init-form 0)
      (bedrooms :request-type :POST :parameter-type 'integer :init-form 0)
      (bathrooms :request-type :POST :parameter-type 'integer :init-form 0)
      (terrace-p :request-type :POST :parameter-type 'integer :init-form 0)
      (garden-p :request-type :POST :parameter-type 'integer :init-form 0)
      (parking-lots :request-type :POST :parameter-type 'integer :init-form 0)
      (building-permit-p :request-type :POST :parameter-type 'integer :init-form 0)
      (destination :request-type :POST :parameter-type 'string :init-form "")
      (summons :request-type :POST :parameter-type 'string :init-form "")
      (preemption :request-type :POST :parameter-type 'string :init-form "")
      (subdiv-permit :request-type :POST :parameter-type 'string :init-form "")
      (epc :request-type :POST :parameter-type 'string :init-form 0)
      (kad-ink :request-type :POST :parameter-type 'string :init-form 0)))
  (if (session-value 'logged-in-p)
      (let* ((save-e
	      (make-instance
	       'estate :ix-user (ix-user (session-value 'user-authed))
	       :address address :telnum telnum :visible (> visible 0)
	       :ix-main-pic ix-main-pic :loc-lat loc-lat :loc-lng loc-lng
	        
	       :apt-type apt-type :status status :pst-code pst-code :munic munic 
	       :ix-country ix-country :constr constr :total total :land land 
	       :desc desc :zmh zmh :price price :since since :bedrooms bedrooms 
	       :bathrooms bathrooms :terrace-p (> terrace-p 0) 
	       :garden-p (> garden-p 0) :parking-lots parking-lots 
	       :building-permit-p (> building-permit-p 0)
	       :destination destination :summons summons :preemption preemption 
	       :subdiv-permit subdiv-permit :epc epc :kad-ink kad-ink))
	     (e-pics
	      (loop for k being the hash-keys of (session-value 'rem-pics)
		 using (hash-value v)
		 collecting v)))
	(destructuring-bind
	      (success-p ix-saved-e error-message)
	    (save-estate-and-pics ix-estate save-e (session-value 'rem-pics);e-pics 
				  :main-pic-uuid main-pic-uuid)
	  ;;if saved successfully, remove images in session
	  (if success-p (setf (session-value 'rem-pics) nil))
	  error-message))
      "Not logged in!"))

(htoot-handler (estate-form-pic-box-handler 
		"/estate-form-pic-box" 
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
	    (if (< 0 ix-pic) (setf (ix-pic pic-to-rem) ix-pic))
	    (cl-fad:copy-file path path-tmp :overwrite t)
	    (setf (gethash uniq-rem-pic-uuid (session-value 'rem-pics)) 
		  pic-to-rem)
	    (estate-form-pic-box uniq-rem-pic-uuid))))))

(defun link-for-estate (e)
  (smake "/#estate-" (ix-estate e) 
	 "-" (apt-type e)
	 "-for-" (status e)
	 "-in-" (pst-code e) "-" (munic e) "-" (get-country (ix-country e))
	 
	 ))

(defun estate-for-json (e &key short)
  (let ((is-fav (and (session-value 'logged-in-p)
		     (session-value 'user-authed)
		     (user-has-fav (ix-user (session-value 'user-authed))
				   (ix-estate e))
		     t))
	;;broker is the user who authored this estate
	(broker (with-re-db (get-dao 'user (ix-user e)))))
    (json:encode-json-plist-to-string
     (if short
	 (list :ix-estate (ix-estate e)
	       ;;:ix-user (ix-user e)
	       :main-pic (pic-to-hashtable (ix-main-pic e)
					   :make-path-linkable t)
	       :can-fav (if (session-value 'logged-in-p) t)
	       :is-fav is-fav
	       :link (link-for-estate e))
	 (list :ix-estate (ix-estate e)
	       :ix-user (ix-user e)
	       :main-pic (pic-to-hashtable (ix-main-pic e)
					   :make-path-linkable t)
	       :other-pics (mapcar (lambda (p)
				     (pic-to-hashtable 
				      p :make-path-linkable t))
				   (estate-nonmain-pics e))
	       :fields (estate-to-hash-table e)
	       :loc-lat (loc-lat e)
	       :loc-lng (loc-lng e)
	       :can-fav (if (session-value 'logged-in-p) t)
	       :is-fav is-fav
	       :link (link-for-estate e)
	       :broker-url (if broker (url broker) "")
	       :broker-logo (broker-logo-url (ix-user e))
	       :has-edit-link 
	       (if (and (session-value 'logged-in-p)
			(session-value 'user-authed)
			(= (ix-user e)
			   (ix-user (session-value 'user-authed))))
		   "true"
		   "false"))))))

(htoot-handler 
    (get-estate-handler "/get-estate" 
			((id :parameter-type 'integer)))
  (with-re-db
    (let ((e (get-dao 'estate id)))
      (if e
	  (estate-for-json e)
	  "{}"))))

(htoot-handler 
    (filter-handler "/filter" 
		    ((preds :init-form "[]")
		     (short :parameter-type 'keyword :init-form nil)
		     (count :parameter-type 'integer :init-form 33)
		     (offset :parameter-type 'integer :init-form 0)))
  (let ((pred (json:decode-json-from-string preds))) 
    (with-re-db 
      (json:encode-json-to-string 
       (mapcar (lambda (e)
		 (estate-for-json e :short short))
	       (filter-estates pred :count count :offset offset))))))
;;pred is an alist like ((:APT-TYPE "new")(:STATUS "sale"))
;;apt-type filter can be gotten using (cdr (assoc :apt-type pred))

(htoot-handler (contact-page-handler "/contact" ())
  (contact-page))

(htoot-handler 
    (set-fav-handler 
     "/set-fav-handler" 
     ((ixestate :parameter-type 'integer :init-form 0)
      (shouldexist :parameter-type 'integer :init-form 0)))
  (let ((action (if (< 0 shouldexist) "add-fav" "remove-fav")))
    (if (and (< 0 ixestate)
	     (session-value 'logged-in-p)
	     (session-value 'user-authed))
	(progn (if (< 0 shouldexist)
		   (add-fav (ix-user (session-value 'user-authed))
			    ixestate)
		   (remove-fav (ix-user (session-value 'user-authed))
			       ixestate))
	       (json:encode-json-plist-to-string 
		(list :result "success" 
		      :action action)))
	(json:encode-json-plist-to-string 
	 (list :result "failure" 
	       :action action)))))

