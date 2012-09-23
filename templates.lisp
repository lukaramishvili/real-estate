(ql:quickload 'cl-who)
;;(ql:quickload 'closer-mop)

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
     (:meta :http-equiv "Content-Type" 
	    :content "text/html; charset=utf-8")
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
	  (reduce #'(lambda (arg1 arg2) 
		      (concatenate 'string arg1 arg2))
		  (mapcar 
		   #'(lambda (js-file) 
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
		#'(lambda (form) 
		    (if (symbolp form) (getf ,row form) form))
		,@body)
	       (if other-rows
		   (do-row (car other-rows) (cdr other-rows))
		   nil))))
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
		   nil))))
     (do-obj (car ,objs) (cdr ,objs))))

;;;do class instance list like this:
;;;(do-objects-to-s (*companies* obj) `("<td class='name'>" name-geo "</td>"))
;;;where *companies* is a list of class instances and 'name-geo is a slot
(defmacro do-objects-to-s ((objs obj) &body body)
  `(smake (do-objects (,objs ,obj) ,@body)))

(defmacro for-each-class-slot 
    ((class-name-sym slot slot-type) &body body)
  `(labels 
       ((do-slots (slots)
	  (let* ((,slot (car slots)) 
		 (,slot-type (slot-value ,slot 'type)))
	    (append (list ,@body) 
		    (if (cdr slots) (do-slots (cdr slots)))))))
     (do-slots (closer-mop:class-slots 
		(find-class ,class-name-sym)))))

(defun script-tag (code)
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue nil :indent t)
    (:script :type "text/javascript"
	     (cl-who:str code))))

(defun style-tag (code)
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue nil :indent t)
    (:style :type "text/css"
	     (cl-who:str code))))

(defun label-input (name &key val label (type "text") (size-attr ""))
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue nil :indent t)
    (:label :for (+s "input_" name)
	    :id (+s "label_" name) :class "label-left"
	    (cl-who:str (or label name)))
    (:input :type type :id (+s "input_" name)
	    :value (or (smake val) "")
	    :name name :size size-attr)))

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
  "makes <label..><select><option..>*</>. if direct-selectbox is passed, 
  its inserted instead of generated select tag. options can be either 
  ((val lbl) (val lbl)), or (opt opt); on lack of lbl, opt will be used."
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue nil :indent t)
    (:label :for (+s "select_" name)
	    :id (+s "label_" name) :class "label-left"
	    (cl-who:str name))
    (if direct-selectbox
	(cl-who:str direct-selectbox)
	(cl-who:htm 
	 (:select 
	  :id (+s "input_" name) :name name
	  (loop for option in options
	     do (let ((opt-val (if (listp option) 
				   (car option) 
				   option))
		      (opt-lbl (if (listp option) 
				   (or (cadr option) (car option)) 
				   option)))
		  (if (equal opt-val val)
		      (htm (:option :value opt-val :selected "selected" 
				    (str opt-lbl)))
		      (htm (:option :value opt-val (str opt-lbl)))))))))))

(defun selectbox-from-class (&key select-name class-name value-slot 
			     label-slot not-selected-option)
  "gen <select> from class items. if passed, first item val will be not-sel.."
  (cl-who:with-html-output-to-string
      (*standard-output* nil :prologue nil :indent t)
    (:select
     :name select-name
     (if not-selected-option
	 (cl-who:htm (:option :value not-selected-option 
			      "Not Selected")))
     (loop for item in (all-of-class class-name)
	collecting 
	  (cl-who:htm (:option 
		       :value (slot-value item value-slot)
		       (cl-who:str 
			(slot-value item label-slot))))))))

(defun label-datepicker
    (name &key (val (get-universal-time)) label)
  (let ((value (* 1000 (unix-time-from-universal val))))
    (+s (label-input (+s "datepicker_" name) :label (or label name))
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



(defun fb-like-btn (url &key (w 170) (h 35))
  (+s "<iframe src='//www.facebook.com/plugins/like.php?href=" url 
      "&amp;send=false&amp;layout=standard&amp;width=" w 
      "&amp;show_faces=false&amp;action=like&amp;colorscheme=dark&amp;" 
      "font=arial&amp;height=" h "' scrolling='no' frameborder='0' " 
      " style='border:none; overflow:hidden; width:" w "px; height:" h 
      "px;' allowTransparency='true'></iframe>"))


(defun fb-share-btn (url &key (caption "Share On Facebook"))
  (+s "<a rel='nofollow' href='http://www.facebook.com/share.php?u=" url
      "' onclick='return fbs_click()' target='_blank' class='fb_share_link'>"
      caption "</a>"))

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
	:css-files '("css/smoothness/jquery-ui-1.8.21.custom.css"
		     "css/reset.css" "css/elements.css" 
		     "css/fancybox/jquery.fancybox.css"
		     "css/re.css" "re-gen.css")
	:js-files '("js/jquery-1.7.2.min.js" 
		    "js/jquery-ui-1.8.21.custom.min.js"
		    "js/jquery.fancybox.pack.js"
		    "js/jquery.mousewheel.min.js"
		    "http://maps.googleapis.com/maps/api/js?key=AIzaSyDl2UEh2szaf3AjDf24cj4AFN-7a0oIUM0&sensor=false"
		    "main.js")
	:more "<meta name=\"viewport\" content=\"initial-scale=1.0, user-scalable=no\" />"))

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
	 (user-estates (estates-of-user ix-user)))
    (cl-who:with-html-output-to-string
	(*standard-output* nil :prologue nil :indent t)    
      (cl-who:str (style-tag (style-account-page)))
      (:h1 (cl-who:str (+s "User: " (username acc-user))))
      (if 
       (plusp (length user-estates))
       (cl-who:htm
	(:div
	 :id "user-estates"
	 (:h2 "Real estates by you")
	 (loop for e in user-estates
	    do (cl-who:htm
		(:p (:a :href (smake "./#estate-" (ix-estate e))
			(cl-who:str (desc e)))
		    (:a :href (smake "./edit-estate?ix-estate="
				     (ix-estate e))
			"- Edit"))))))))))


(defun register-page (&key reg-token acc-type div-id)
  (let ((checked-type (if (valid-acc-type-p acc-type) 
			  acc-type "simple")))
    (cl-who:with-html-output-to-string 
	(*standard-output* nil :prologue nil :indent t)
      (cl-who:str (style-tag (style-register-page)))
      (:div 
       :id (or div-id "reg-div") :class "reg-div"
       (:form 
	:method "post" :action "./register-handler"
	:autocomplete "off" :enctype "multipart/form-data"
	(:h1 "Register")
	(:input :type "hidden" :name "reg-token" :value reg-token)
	(:input :type "hidden" :name "acc-type" :value checked-type)
	(cl-who:str
	 (+s
	  (label-input "usr" :label "Username:" :val "")
	  (label-input "email" :label "Email address:" :val "")
	  (label-input "pwd" :label "Password:" :type "password" :val "")
	  (label-input "fname" :label "First name:" :val "")
	  (label-input "lname" :label "Last name:" :val "")
	  (if (string-equal checked-type "broker")
	      (+s 
	       (label-input "url" :label "Website:")
	       (label-input "telnum" :label "Tel:")
	       (label-input "logo" :type "file"
			    :label "Upload your logo:"))
	      "")))
	(:input :type "submit" :value "Register"))
       (:br :class "clearfloat")
       (:div (cl-who:str
	    (+s "By registering, you will have access to numerous<br>"
		"features, such as favoriting real estate properties.")))))))


(defun login-page (&key (redir "/"))
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue nil :indent t)
    (cl-who:str (style-tag (style-login-page)))
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
		     (cl-who:str (re-tr :register-link)))))))

;;; project-specific code

(defun uneven-grid (w h &key square)
  (loop for iw from 0 to (- w 1)
     collecting
       (loop for ih from 0 to (- h 1)
	  collecting :min)));; :min :none :square

(defun fill-grid (grid bucket)
  "grid should be ret'ed by uneven-grid; 
   bucket should be (list :minimals (list ...) :squares (list ...))"
  (let ((h (length grid))
	(w (length (car grid)))
	(ret (copy-tree grid))
	(minimals (getf bucket :minimals))
	(squares (getf bucket :squares)))
    (loop for ih from 0 to (- h 1)
       do
	 (loop for iw from 0 to (- w 1)
	    do (let ((current (nth iw (nth ih ret))))
		 (case current
		   (:min (setf (nth iw (nth ih ret)) 
			       (or (pop minimals) "")))
		   (:square 
		    (setf (nth iw (nth ih ret)) 
			  (or (pop squares) "")))
		   (:none (setf (nth iw (nth ih ret)) ""))))))
    ret))

;;;old version of #fp-pics contents
#+nil(cl-who:str
      (with-re-db
	(let* 
	    ((normal-size-pics 
	      (mapcar 
	       (lambda (p) 
		 (+s 
		  "<a href='#view-estate' class='fp-estate-link' " 
		  " ixestate='" (ix-estate p) "'" 
		  " style='background:transparent url(" 
		  (linkable-pic-path p)") center center no-repeat;' " 
		  ">"
		  ;;"<img src='" (linkable-pic-path p) "' class='img-min' />" 
		  "</a>"))
	       (pics-for-firstpage)))
	     (2x2-size-pics 
	      (mapcar (lambda (p) 
			(+s  "<img src='" (linkable-pic-path p) 
			     "' class='img-sq' />"))
		      (pics-for-firstpage)))
	     (grid-width 5);;decide width based on total img count
	     (grid-height 5)
	     (grid (uneven-grid grid-width grid-height))
	     (filled-grid (fill-grid grid (list :minimals normal-size-pics 
						:squares 2x2-size-pics))))
	  (apply 
	   #'+s 
	   (loop for i from 0 to (1- (length filled-grid))
	      collecting 
		(+s "<div class='grid-10'>" 
		    (reduce #'+s (nth i filled-grid)) "</div>"))))))

(defun re-firstpage ()
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue nil :indent t)
    (:div 
     :id "top-menu"
     (:a :href "javascript:toggleSearchBar();" :id "top-search-btn" "Search")
     (if 
      (session-value 'logged-in-p)
      (cl-who:htm
       (:a :href "./account" :id "top-account-link" :class "fancybox.iframe" 
	   (cl-who:str (+s "Logged in as " 
			   (username (session-value 'user-authed)))))
       (:a :href "./logout" :id "top-logout-link" "Log out"))
      (cl-who:htm
       (:a :href "#login-form-div" :id "top-login-link" 
	   :class "fancybox.inline" "Login")
       (:a :href "#reg-div" :id "top-reg-link" :class "fancybox.inline" 
	   "Register")
       (:a :href "#reg-broker-div" :id "top-reg-broker-link"
	   :class "fancybox.inline" "Register as Broker")))
     (:a :href "./contact" :id "top-contact-link" :class "fancybox.iframe" 
	 "Contact"))
    (:div :id "fp-pics"
	  (:table :id "fp-pics-table" 
		  :border 0 :cellpadding 0 :cellspacing 0
		  (:tr :id "fp-pics-table-tr")))
    (:img :id "fp-preloader" :src "css/img/preloader.gif")
    (:div :id "view-estate"
	  (:a :id "e-close-btn" 
	      :href "javascript:hideEstateDiv();void(0)")
	  (:div :id "view-estate-inner"))
    (:div 
     :id "search-bar"
     (:a :href "javascript:void(0)" :id "btn-toggle-search"
	 "&nbsp;")
     (cl-who:str 
      (+s
       (if (session-value 'logged-in-p)
	   (+s
	    (label-checkbox "only-my-estates" 
			    :label "Only properties added by me"))
	   "")
       (label-select "status" :options (status-options :not-sel t))
       (label-select "apt-type" :options (apt-type-options :not-sel t))
       (label-select "ix-country" :options (all-countries :not-sel t))
       (label-input "price-min" :label "Price: between " :size-attr 4)
       (label-input "price-max" :label " and " :size-attr 4)
       (label-input "bedrooms-min" :label "No. bedrooms: from " :size-attr 4)
       (label-input "bedrooms-max" :label " to " :size-attr 4)
       ))
     (:a :href "javascript:void(0);" :id "btn-toggle-adv-search"
	 "More filters")
     (:div 
      :id "search-adv"
      (cl-who:str
       (+s 
	(label-input "bathrooms-min" :label "No. bathrooms: from " :size-attr 4)
	(label-input "bathrooms-max" :label " to " :size-attr 4)
	(label-checkbox "garden" :val 1)
	(label-input "total-min" :label "Habitable surface: min. " :size-attr 2)
	(label-input "total-max" :label " max. " :size-attr 3)
	(label-input "land-min" :label "Land area: min. " :size-attr 3)
	(label-input "land-max" :label " max. " :size-attr 4)
	(label-input "epc-max" :label "EPC: max. " :size-attr 4)
	(label-select "constr" :options (constr-options :not-sel t))
	(label-checkbox "terrace" :val 1)
	(label-checkbox "building-permit" :val 1)
	(label-select "summons" :options (summons-options :not-sel t))
	(label-select "preemption" :options (preemption-options :not-sel t))
	(label-select "subdiv-permit" 
		      :options (subdiv-permit-options :not-sel t))
	))))
    (cl-who:str (script-tag (fp-search-js)))
    (:div :id "ajax-pages"
	  (cl-who:str 
	   (+s (register-page :acc-type "broker" :div-id "reg-broker-div")
	       (register-page)
	       (login-page))))))


;;TODO: populate a grid (generated with uneven-grid) with fp-pics
#+nil(loop for img in normal-size-pics
	do (cl-who:htm 
	    (:div 
	     :class "grid-10"
	     (cl-who:str 
	      (+s "<img src='" (linkable-pic-path img) 
		  "' /><br>")))))


(defun estate-edit-form (e)
  (let ((ix-estate (if (slot-boundp e 'ix-estate)
		       (ix-estate e) 0)))
    (cl-who:with-html-output-to-string 
	(*standard-output* nil :prologue nil :indent t)
      (:div 
       :id "edit-estate-form-div"
       (:form 
	:method :post :action "./save-estate"
	(:input :type "hidden" :name "ix-estate" :value ix-estate)
	;;not useful, because 
	(:input :type "hidden" :name "ix-main-pic" 
		:value (ix-main-pic e))
	(:input :type "hidden" :name "main-pic-uuid" 
		:id "input_main-pic-uuid" :value "")
	(:h1 "Edit real estate")
	(:div 
	 :class "edit-estate-column"
	 (cl-who:str
	  (+s 
	   (label-input "telnum" :val (telnum e))
	   (label-select "apt-type" 
			 :options (apt-type-options)
			 :val (apt-type e))
	   (label-select "status" :options (status-options)
			 :val (status e))
	   (label-input "pst-code" :val (pst-code e))
	   (label-input "munic" :val (munic e))
	   (label-select "ix-country" :options (all-countries)
			 :val (ix-country e))
	   (label-select "constr" :options (constr-options)
			 :val (constr e))
	   (label-input "total" :val (total e) :label "Total m2")
	   (label-input "land" :val (land e) :label "Land area m2")
	   (label-input "desc" :val (desc e) :label "Description")
	   (label-input "zmh" :val (zmh e) :label "ZMH Reference")
	   (label-input "price" :val (price e))
	   (label-datepicker "since" :val (since e) :label "Date added"))))
	(:div 
	 :class "edit-estate-column"
	 (cl-who:str
	  (+s 
	   (label-input "bedrooms" :val (bedrooms e) :label "Bedroom count")
	   (label-input "bathrooms" :val (bathrooms e) :label "Bathroom count")
	   (label-checkbox "terrace-p" :val 1 :checked (< 0 (terrace-p e)))
	   (label-checkbox "garden-p" :val 1 :checked (< 0 (garden-p e)))
	   (label-input "parking-lots" :val (parking-lots e))
	   (label-input "building-permit-p" :val (building-permit-p e))
	   (label-input "destination" :val (destination e))
	   (label-select "summons" :options (summons-options) 
			 :val (summons e))
	   (label-select "preemption" :options (preemption-options)
			 :val (preemption e))
	   (label-select "subdiv-permit" :options (subdiv-permit-options)
			 :val (subdiv-permit e))
	   (label-input "epc" :val (epc e) :label "EPC")
	   (label-input "kad-ink" :val (kad-ink e) :label "K.I.")
	   (label-checkbox "visible" :val 1 :checked (< 0 (visible e))))))
	(:div :class "edit-estate-column"
	      (:div :id "estate-pics")
	      (:button :id "add-estate-pic" :type "button" "Add image"))
	(:div 
	 :class "edit-estate-column"
	 (cl-who:str (label-input "address" :val (address e)))
	 (:h4 "write address in the box or click on the map " 
	      "to set location")
	 (:input :type "hidden" :id "loc-lat" :name "loc-lat" :value (loc-lat e))
	 (:input :type "hidden" :id "loc-lng" :name "loc-lng" :value (loc-lng e))
	 (:div :id "edit-estate-map"))
	(:br :class "clearfloat")
	(:input :type "submit" :value "Save")))
      (:script
       :type "text/javascript"
       (cl-who:str
	(+s (ps:ps
	      (defvar estate-map (create-map-for-id "edit-estate-map"))
	      (defvar loc-marker 
		(create-marker "Real estate map location"
			       (new (google.maps.-lat-lng 
				     (lisp (loc-lat e)) 
				     (lisp (loc-lng e))))))
	      (chain loc-marker (set-map estate-map))
	      (google.maps.event.add-listener 
	       estate-map "click" 
	       (lambda (event)
		 (let ((lat (event.lat-lng.lat)) (lng (event.lat-lng.lng)))
		   (chain ($ "#loc-lat") (val lat))
		   (chain ($ "#loc-lng") (val lng))
		   (chain loc-marker
			  (set-position
			   (new (google.maps.-lat-lng lat
						      lng))))
		   t)))
	      (defun set-main-pic (pic-uuid)
		($$ "#input_main-pic-uuid" (val pic-uuid))
		false)
	      (defun next-avail-iframe-id ()
		(let ((max-id 0))
		  ($$ 
		   "iframe[id|='pic_iframe']"
		   (each 
		    (lambda (i el)
		      (setf max-id 
			    (-math.max 
			     max-id 
			     (aref (chain ($$ el (attr "id")) 
					  (to-string) (split "-")) 1))))))
		  (+ max-id 1)))
	      (defun add-pic-box (rem-pic-uuid)
		(let ((next-id (next-avail-iframe-id)))
		  (chain 
		   ($ "#estate-pics")
		   (append 
		    (ps:who-ps-html
		     (:div :class "estate-pic" :id (+ "pic_div-" next-id)
			   (:iframe :src (+ "/estate-form-pic-box?rem-pic-uuid="
					    rem-pic-uuid)
				    :id (+ "pic_iframe-" next-id))
			   (:a :href (+ "javascript:void(0);")
			       :id (+ "set_main_pic_btn-" next-id)
			       "Set as main pic")))))))
	      ($$ "#add-estate-pic" 
		  (click (lambda () (add-pic-box "") false)))
	      ($$ "a[id|='set_main_pic_btn']"
		  (live "click" 
			(lambda ()
			  (set-main-pic ($$ this (attr "rem-pic-uuid"))))))
	      (-map-marker-A-C-Combo "#input_address" "#loc-lat" "#loc-lng"
				     estate-map loc-marker)
	      );end main ps:ps
	    (smake
	     (loop for key being the hash-keys 
		of (session-value 'rem-pics)
		collecting 
		  (let ((p (gethash key (session-value 'rem-pics))))
		    (smake "addPicBox('" key "');" 
			   (if (and
				(slot-boundp p 'ix-pic)
				(= (ix-main-pic e) 
				   (ix-pic p)))
			       (smake "setMainPic('" key "');")
			       "")))))
	    ))))))

(defun estate-form-pic-box (&optional (rem-pic-uuid ""))
  (let ((saved-pic (gethash rem-pic-uuid (session-value 'rem-pics))))
    (cl-who:with-html-output-to-string 
	(*standard-output* nil :prologue nil :indent t)
      (:script :type "text/javascript" :src "/js/jquery-1.7.2.min.js")
      (:style :type "text/css" (cl-who:str (style-pic-box-iframe)))
      (:form 
       :action "/rem-pic" :method :post :enctype "multipart/form-data"
       :class "form-in-pic-box-iframe"
       (:div
	:class "div-in-pic-box-iframe"
	(:input :type "hidden" :name "rem-pic-uuid" :value rem-pic-uuid)
	(if (and saved-pic
		 (slot-boundp saved-pic 'ix-pic)
		 (plusp (ix-pic saved-pic)))
	    (cl-who:htm (:input :type "hidden" :name "ix-pic" 
				:value (ix-pic saved-pic))) 
	    (cl-who:str ""))
	(:img :src (if saved-pic (linkable-tmp-path (path saved-pic)) 
		       "/css/img/no-pic.jpg"))
	(cl-who:str 
	 (label-input "img" :type "file" :label "Choose Image:" 
		      :size-attr 10))
	;;(:input :type "submit" :value "Update image")
	(:script
	 :type "text/javascript"
	 (cl-who:str 
	  (ps:ps
	    (chain
	     ($ "[name='img']")
	     (change 
	      (lambda
		  () (chain ($ ".form-in-pic-box-iframe")
			    (submit)
			    ))))
	    (when (< 0 (chain (ps:lisp rem-pic-uuid) length))
	      ;;$(window.parent.document).find("iframe").each(function(i, el){if((el.contentWindow || el.contentDocument) == window){/*here, el is the iframe#pic_iframe-3, so show a#set_main_pic_btn-3*/};});
	      ($$ window.parent.document
		  (find "iframe")
		  (each 
		   (lambda (i el)
		     (when (== (or (@ el content-window)
				   (@ el content-document))
			       window)
		       (let ((pic-id 
			      (aref (chain ($$ el (attr "id")) 
					   (to-string) (split "-")) 1)))
			 ($$ el (parents "body")
			     (find (+ "#set_main_pic_btn-" pic-id)) 
			     (attr "rem-pic-uuid" (lisp rem-pic-uuid))
			     (show))))))))
	    ))))))))

(defun contact-page ()
  (cl-who:with-html-output-to-string 
      (*standard-output* nil :prologue nil :indent t)
    (cl-who:str (style-tag (style-contact-page)))
    (:div 
     :id "contact-div"
     (:h1 "Contact us")
     (:div :class "text" 
	   "You can contact us by calling +995 11 22 33"))))

