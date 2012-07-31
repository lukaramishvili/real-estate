(in-package :re)

(ql:quickload :css-lite)

(defun abs-pos (div x y w h)
  (css-lite:css ((div) ((:position "absolute")
	       (:left x) (:top y) (:width w) (:height h)))))

(defun style-firstpage ()
  "
  
  ")

(defun style-edit-estate-form ()
  "
  #edit-estate-form-div { padding:20px; }
  #edit-estate-form-div iframe { border:0; height:auto; width:auto; 
    width:400px; height:150px;
  }
  #edit-estate-map { width:300px; height:300px; }
  
  #edit-estate-form-div h4 { clear:both; }
  #edit-estate-form-div input,#edit-estate-form-div label
   ,#edit-estate-form-div select { float:left; display:block;
      margin-bottom:4px; }
  #edit-estate-form-div label { width:150px; }
  #edit-estate-form-div label.label-left { clear: left; }
  #edit-estate-form-div label.label-right { clear: right; }
  #edit-estate-form-div input[type='checkbox'] 
      { clear:left; margin-right:135px; }
  #edit-estate-form-div input[type='submit'] 
      { clear:both; }
  
  #estate-pics { clear:both; }
  ")
(defun style-pic-box-iframe()
  "
  .div-in-pic-box-iframe img { max-width:150px; height:80px; }
  ")

(defun re-gen-css ()
  (+s
   (css-lite:css
     (("body") ((:height "100%")))
     (("#main-container") ((:position "absolute") (:left "0px") 
			   (:top "0px") (:width "100%" :height "100%")))
     (("#main") ((:position "absolute") (:left "0px") (:top "0px")
		 (:width "100%" :height "100%")))
     ((".grid-10") ((:width "10%") (:float "left") (:height "100%"))))
   (style-firstpage)
   (style-edit-estate-form)))