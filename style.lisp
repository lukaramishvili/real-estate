(in-package :re)

(ql:quickload :css-lite)

(defun abs-pos (div x y w h)
  (css-lite:css ((div) ((:position "absolute")
	       (:left x) (:top y) (:width w) (:height h)))))

(defun re-gen-css ()
  (css-lite:css
    (("body") ((:height "100%")))
    (("#main-container") ((:position "absolute") (:left "0px") (:top "0px")
			  (:width "100%" :height "100%")))
    (("#main") ((:position "absolute") (:left "0px") (:top "0px")
			  (:width "100%" :height "100%")))
    ((".grid-10") ((:width "10%") (:float "left") (:height "100%")))))