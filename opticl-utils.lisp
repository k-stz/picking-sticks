(defpackage :opticl-utils
  (:use :cl
	:opticl)
  (:export :image-file->image-object
	   ;; image-objects
	   :width
	   :height
	   :pixels
	   :pos-ffi-array))

(in-package :opticl-utils)




;;sdl2-image experiments--------------------------------------------------------

;; (sdl2-image:init '(:png :jpg :tif))


;; TODO add more useful sdl-surface struct fields. type information?
(defclass image-object ()
  ((height :initarg :height)
   (width :initarg :width)
   (pixels :initarg :pixels)
   (pos-ffi-array)))

(defmethod initialize-instance :after ((the-image image-object) &key)
  (alexandria:if-let ((pixels (slot-value the-image 'pixels)))
    (setf (slot-value the-image 'pos-ffi-array)
  	  ;; TODO: get type from #3Array?
  	  (cffi:foreign-alloc :unsigned-char :initial-contents pixels))
    ;; else
    (error "The PIXELS slot is not set in the IMAGE-OBJECT object.")))


;; TODO: chedk out 3bgl-opticl for a direct solution
;; TODO: check out sbcl for some lowlevel vector access and provide it with
;;       #+sbcl.. ?
(defun 3d-array->vector (3d-array)
  (let* ((dims (array-dimensions 3d-array))
	 (dims-1d (apply #'* dims))
	 (vector (make-array dims-1d)))
    (loop for i below dims-1d do
	 (setf (aref vector i) (row-major-aref 3d-array i)))
    vector))


(defun image-file->image-object (path &optional (format :png))
  ;; TODO: bad style?
  ;; special-variables are special for this reason:
  (let* ((*default-pathname-defaults* (asdf/system:system-source-directory "picking-sticks"))
	 (img (convert-image-to-rgba
	       (ecase format
		 (:png (read-png-file path))
		 (:jpeg (read-jpeg-file path))
		 (:tiff (read-tiff-file path))))))
    ;; first dim: height then width then length of pixel, here 4 because rgba
    (with-image-bounds (height width) img
      (make-instance 'image-object :height height :width width
		     :pixels (3d-array->vector img)))))



;;useful functions
;; (with-image-bounds (height width) *png-opticl*
;;   (list height width)) ; ==> (3 3)
;; (convert-image-to-rgba *PNG-OPTICL*)

;; since we're dealing with a simple array
;; (array-element-type <array>)
;; (array-dimensions <array>)
