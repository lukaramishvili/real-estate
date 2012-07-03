(ql:quickload 'cl-who)
;(ql:quickload 'closer-mop)

;;(defpackage :re-templates
;;  (:use :common-lisp :cl-user :closer-mop :cl-who))

(in-package :re)

(defun html-combine (&key head body)
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue t :indent t)
    (:html :xmlns "http://www.w3.org/1999/xhtml"
     (cl-who:str head)
     (:body (cl-who:str body)))))

(defun head (title &key css-files js-files more)
  (cl-who:with-html-output-to-string 
   (*standard-output* nil :prologue nil :indent t)
   (:head
    (:meta :http-equiv "Content-Type" :content "text/html; charset=utf-8")
    (:title (cl-who:str title))
    (cl-who:str
     (if (and css-files (listp css-files))
	 (reduce #'(lambda (arg1 arg2) (+s arg1 arg2))
		 (mapcar #'(lambda (css-file) 
			     (concatenate 
			      'string 
			      "<link rel=\"stylesheet\" href=\"" 
			      css-file "\">"))
			 css-files))
       ""))
    (cl-who:str
     (if (and js-files (listp js-files))
	 (reduce #'(lambda (arg1 arg2) (concatenate 'string arg1 arg2))
		 (mapcar #'(lambda (js-file) 
			     (concatenate 
			      'string 
			      "<script type=\"text/javascript\" src=\"" 
			      js-file "\"></script>"))
			 js-files))
       ""))
    (cl-who:str (smake (or more ""))))))

(defun simple-page (heading text)
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue nil :indent t)
    (:div :class "simple-page-div"
	  (:h5 (cl-who:str heading))
	  (:div :class "text" (cl-who:str text)))))

(defmacro do-table ((rows row) &body body)
  `(labels ((do-row (,row other-rows)
	      (append 
	       (mapcar 
		#'(lambda (form) (if (symbolp form) (getf ,row form) form))
		,@body)
	       (if other-rows
		   (do-row (car other-rows) (cdr other-rows))
		   NIL))))
     (do-row (car ,rows) (cdr ,rows))))

(defmacro do-table-to-s ((rows row) &body body)
  `(smake (do-table (,rows ,row) ,@body)))

(defmacro do-objects ((objs obj) &body body)
  `(labels ((do-obj (,obj other-objs)
	      (append
	       (mapcar 
		#'(lambda (form) 
		    (if (and ,obj (symbolp form))
			(slot-value ,obj form) 
		      form))
		,@body)
	       (if other-objs
		   (do-obj (car other-objs) (cdr other-objs))
		   NIL))))
     (do-obj (car ,objs) (cdr ,objs))))

;;;do class instance list like this:
;;;(do-objects-to-s (*companies* obj) `("<td class='name'>" name-geo "</td>"))
;;;where *companies* is a list of class instances and 'name-geo is a slot
(defmacro do-objects-to-s ((objs obj) &body body)
  `(smake (do-objects (,objs ,obj) ,@body)))

(defmacro for-each-class-slot ((class-name-sym slot slot-type) &body body)
  `(labels 
       ((do-slots (slots)
	  (let* ((,slot (car slots)) (,slot-type (slot-value ,slot 'type)))
	    (append (list ,@body) (if (cdr slots) (do-slots (cdr slots)))))))
     (do-slots (closer-mop:class-slots (find-class ,class-name-sym)))))

(defun script-tag (code)
  (cl-who:with-html-output-to-string 
   (*standard-output* nil :prologue nil :indent t)
   (:script :type "text/javascript"
	    (cl-who:str code))))

(defun label-input (name &key val label (type "text"))
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue nil :indent t)
    (:label :for (+s "input_" name)
	    :id (+s "label_" name) :class "label-left"
	    (cl-who:str (or label name)))
    (:input :type type :id (+s "input_" name)
	    :value (or (smake val) "")
	    :name name)))

(defmacro label-checkbox (name &key val label checked)
  `(cl-who:with-html-output-to-string 
    (*standard-output* nil :prologue nil :indent t)
    (:input :type "checkbox" :id (+s "input_" ,name)
	    :value (or (smake ,val) "")
	    :name ,name
	    ,@(if checked (list :checked "checked")))
    (:label :for (+s "input_" ,name)
	    :id (+s "label_" ,name) :class "label-right"
	    (cl-who:str (or ,label ,name)))))

(defun label-select (name &key options val direct-selectbox)
  "makes <label..><select><option..>*</>. if direct-selectbox
   is passed, its inserted instead of generated select tag"
  (cl-who:with-html-output-to-string 
   (*standard-output* nil :prologue nil :indent t)
   (:label :for (+s "input_" name)
	   :id (+s "label_" name) :class "label-left"
	   (cl-who:str name))
   (if direct-selectbox
       (cl-who:str direct-selectbox)
     (cl-who:htm
      (:select :id (+s "input_" name)
	       :name name
	       (loop for option in options
		     do (htm
			 (:option :value (car option)
				  (if (eql (car option) val)
				      :selected
				    "")
				  (str (cadr option))))))))))

(defun selectbox-from-class (&key select-name class-name value-slot label-slot
				  not-selected-option)
  "gen <select> from class items. if passed, first item val will be not-sel.."
  (cl-who:with-html-output-to-string
   (*standard-output* nil :prologue nil :indent t)
   (:select
    :name select-name
    (if not-selected-option
	(cl-who:htm (:option :value not-selected-option "Not Selected")))
    (loop for item in (all-of-class class-name)
	  collecting 
	  (cl-who:htm (:option 
		       :value (slot-value item value-slot)
		       (cl-who:str (slot-value item label-slot))))))))

(defun label-datepicker
  (name &key (val (get-universal-time)))
  (let ((value (* 1000 (unix-time-from-universal val))))
    (+s (label-input (+s "datepicker_" name))
	(+s "<input type='hidden' name='" name "' "
	    " id='input_" name "' value='" value "' />")
	(script-tag
	 (eval
	  `(ps:ps
	    (chain ($ ,(+s "#input_datepicker_" name))
		   (datepicker
		    (ps:create alt-field ,(+s "#input_" name)
			       alt-format "@")))
	    ""))))))

;;sample usage: 
;;(for-each-class-slot ('foo slot type)
;;  (format nil "next slot type is: ~a" type))

;code reuse
;(defmacro do-table-to-s ((rows row) &body body)
;  `(format nil "~{~a~}" ,`(labels ((do-row (,row)
;	      (append (list ,@body)
;		    (if (cdr ,row) (do-row (cdr ,row)) NIL ))))
;     (do-row ,rows))))

;;;re-specific templates

(defun re-head (&key title)
  (head (or title "Welcome to Project RE!")
	:css-files '("css/reset.css" "css/elements.css" 
		     "css/re.css" "re-gen.css")
	:js-files '("js/jquery-1.7.2.min.js")))

(defun do-menu (items-list)
  (smake (mapcar #'(lambda (item) 
		     (format nil "<a href='~a'>~a</a>"
			     (cadr item)
			     (car item)))
		 items-list)))

(defun re-header (&key lang)
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue nil :indent t)
    (:div :id "header"
	  (:div :id "header-mask")
	  (:a :href "./"
	      (:img :id "logo" :src "../css/img/logo.png"))
	  (:div :id "menu"
		(cl-who:str (do-menu (menu-items 
				      :lang lang))))
	  (if (session-value 'logged-in-p)
	      (cl-who:htm
	       (:div :id "header-account-links"
		     (:a :id "header-account-link" :href "./account"
			 "My account")
		     (:a :id "header-logout-link" :href "./logout"
			 "Logout")))
	    (cl-who:str (login-page))))))









(defun main-template (page)
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue nil :indent t)
    (:div :id "main-container"
	  (:div :id "main"
		;;(cl-who:str (re-header :lang "en"))
		;;(cl-who:str (re-home-search-bar))
		(cl-who:str page)))))

(defun re-main (&key title body)
  (html-combine :head (re-head :title title)
		:body (main-template body)))

(defun account-page (ix-user)
  (let* ((acc-user (single-user ix-user))
	 (acc-company (single-company (ix-company acc-user)))
	 (user-vacancies (filter-vacancies :ix-user (smake ix-user))))
    (cl-who:with-html-output-to-string
	(*standard-output* nil :prologue nil :indent t)
      (if acc-company
	  (cl-who:htm
	   (:p (:a :href (smake "./company?ix-company="
				(ix-company acc-company))))))
      (if (plusp (length user-vacancies))
	  (cl-who:htm
	   (:div
	    :id "user-vacancies"
	    (loop for vac in user-vacancies
	       do (cl-who:htm
		   (:p (:a :href (smake "./vacancy?ix-vacancy="
					(ix-vacancy vac))
			   (cl-who:str (company-name-geo vac)
				       " - " 
				       (name-geo (emp-pos vac))))
		       (:a :href (smake "./edit-vacancy?ix-vacancy="
					(ix-vacancy vac))
			   "Edit"))))))))))






  

(defun register-page (&key reg-token)
  (cl-who:with-html-output-to-string 
   (*standard-output* nil :prologue nil :indent t)
   (:div :id "register-form-div"
	 (:form :method "post" :action "./register-handler"
		(:h1 "Register")
		(:input :type "hidden" :name "reg-token" :value reg-token)
		(cl-who:str
		 (+s
		  (label-input "usr" :label "Username:")
		  (label-input "pwd" :label "Password:" :type "password")))
		(:input :type "submit" :value "Register")))))
  

(defun login-page (&key (redir "/"))
  (cl-who:with-html-output-to-string 
   (*standard-output* nil :prologue nil :indent t)
   (:div :id "login-form-div"
	 (:form :method "post" :action "./login-handler"
		(:input :type "hidden" :name "redir" :value redir)
		(cl-who:str
		 (+s
		  (label-input "usr" :label (re-tr :username))
		  (label-input "pwd" :label (re-tr :password)
			       :type "password")))
		(:input :type "submit" :value (re-tr :btn-log-in))
		(:a :href "./register"
		    (re-tr :register-link))))))

;;; project-specific code

(defun re-firstpage ()
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue nil :indent t)
    (loop for i from 0 to 9
       do (cl-who:htm (:div :class "grid-10"
			    "img <br> img <br>")))))

(defun estate-edit-form (e)
  (let ((ix-estate (if (slot-boundp e 'ix-estate)
		       (ix-estate e) 0)))
    (cl-who:with-html-output-to-string 
	(*standard-output* nil :prologue nil :indent t)
      (:form :method :post :action "./save-estate"
	     (:input :type "hidden" :name "ix-estate" :val ix-estate)
	     (:input :type "hidden" :name "ix-main-pic" :val (ix-main-pic e))
	     (cl-who:str
	      (+s (label-input "address" :val (address e))
		  (label-input "telnum" :val (telnum e))
		  (label-input "visible" :val (visible e))))
	     (:div :id "estate-pics")
	     (:button :id "add-estate-pic" "Add image"))
      (:input :type "submit" :value "Save")
      (:script
       :type "text/javascript"
       (cl-who:str
	(+s (ps:ps
	  (defun add-pic-box (rem-pic-uuid)
	    (chain 
	     ($ "#estate-pics")
	     (append 
	      (ps:who-ps-html
	       (:div :class "estate-pic"
		     (:iframe :src (+ "/estate-form-pic-box?rem-pic-uuid="
				      rem-pic-uuid)))))))
	  (chain ($ "#add-estate-pic")
		 (click (lambda () (add-pic-box "") false)))
	  );end main ps:ps
	    (smake
	     (loop for key being the hash-keys 
		of (session-value 'rem-pics)
		collecting (smake "addPicBox('" key "');")))
	    ))))))

(defun estate-form-pic-box (&optional (rem-pic-uuid ""))
  (let ((saved-pic (gethash rem-pic-uuid (session-value 'rem-pics))))
    (cl-who:with-html-output-to-string 
	(*standard-output* nil :prologue nil :indent t)
      (:form :action "/rem-pic" :method :post :enctype "multipart/form-data"
	     (:input :type "hidden" :name "rem-pic-uuid" :value rem-pic-uuid)
	     (:img :src (if saved-pic (linkable-tmp-path (path saved-pic)) 
			    "/css/img/no-pic.jpg"))
	     (cl-who:str 
	      (label-input "img" :type "file" :label "Choose Image:"))
	     (:input :type "submit" :value "Update image")))))