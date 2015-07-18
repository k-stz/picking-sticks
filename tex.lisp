(defpackage :tex
  (:use :cl))

(in-package :tex)


(defvar *resources-dir* (merge-pathnames "resources/" (asdf/system:system-source-directory "picking-sticks")))

(defun png-bounds (png-filename &optional (directory *resources-dir*))
  (let ((png-filename (merge-pathnames png-filename directory)))
    (cairo:with-png-surface (png-filename surface)
      (let ((width (cairo:width surface))
	    (height (cairo:height surface)))
	(values width height)))))

(defmacro with-png-bounds (width-var height-var
			   (png-filename &optional (directory *resources-dir*))
			   &body body)
  `(multiple-value-bind (,width-var ,height-var)
       (png-bounds ,png-filename ,directory)
     ,@body))

;; This is not how TEXATL works
;; (defun png->spritesheet (png-filename &optional (directory *resources-dir*))
;;   (let ((png-filename (merge-pathnames png-filename directory)))
;;     (with-png-bounds width height (png-filename directory)
;;       (texatl:make-sprite-atlas
;;        width height
;;        (list png-filename)))))
