;; Idea: one array of static rectangles (background)
;;       and one array of constantly changing rectangles
;;      => write all into one vbo (gl:buffe-data ...)
;;      - draws all with one call
;;      - much better performance, less overhead with individual
;;      - gl:buffer-sub-data 
;;
;;      => change rectangle data (position,color) on cpu/cl side
;;         using simple arrays

;; TODO: texatl to get sprites and spritesheet integretation, maybe even text!!

;; one big texture to read all the data from, again using texatl?



(defpackage :game-objects
  (:use :cl
	;; oooooh, only the EXTERNAL symbols get inherited!!
	:kit.gl.shader
	:kit.math)
  (:export :rectangle
	   ;; wow, else we can't access the unqualified from other
	   ;; packages like so (slot-value *rectangle* 'x1) ...
	   :x1 :x2 :y1 :y2
	                 
	   :make-rectangle))

(in-package :game-objects)


(defclass image-object ()
  ((height :initarg :height)
   (width :initarg :width)
   (pixels :initarg :pixels)
   (ffi-array)))


(defvar *recs* nil)

(defclass rectangle ()
  ((x1 :initarg :x1 :type vec2)
   (x2 :initarg :x2 :type vec2)
   (y1 :initarg :y1 :type vec2)
   (y2 :initarg :y2 :type vec2)))

(defun make-rectangle (&optional
			 (position (vec2 0.0 0.0))
			 (width 1.0)
			 (height 1.0))
  (macrolet ((vec2+ (v1 v2)
	       `(vec2 (+ (aref ,v1 0) (aref ,v2 0))
		     (+ (aref ,v1 1) (aref ,v2 1)))))
    (make-instance 'rectangle
		   :x1 position
		   :x2 (vec2+ position (vec2 width 0.0))
		   :y1 (vec2+ position (vec2 0.0 height))
		   :y2 (vec2+ position (vec2 width height)))))




(defvar *default-ffi-positions*
  (cffi:foreign-alloc :float :initial-contents
		      #(0.5 0.5 0.5 0.5 -0.5 0.5 -0.5 -0.5 0.5
			-0.5 -0.5 0.5 -0.5 0.5 0.5 0.5 0.5 0.5)))

(defvar *default-ffi-tex-coordinates*
  (cffi:foreign-alloc :float :initial-contents
		      #(1.0 0.0   1.0 1.0   0.0 1.0
			0.0 1.0   0.0 0.0   1.0 0.0)))

;; ;; TODO: allow to make rectangle-objects without gl-context? Maybe add function
;; ;; to build rectangle-object "on-gpu" I think that's how CEPL's gpu-* functions
;; ;; are named like that
;; (defun make-gpu-rectangle ()
;;   "Returns VAO and VBO for a GPU rectangle data with vbo containing position and texture
;; Data"
;;   ;; TODO: if works, work on inder-buffer-object
;;   (let ((vao (first (gl:gen-vertex-arrays 1)))
;; 	(vbo (first (gl:gen-buffers 1))))
;;     (gl:bind-vertex-array vao)
;;     (gl:bind-buffer :array-buffer vbo)
;;     ;; Positions:
;;     ;; 2 triangles * 3 vertices * 3 components xyz * of 4 bytes (float) = 72
;;     ;; textures:
;;     ;; 2 triangles * 3 vertices * 2 components * of 4 bytes (float) = 48
;;     (%gl:buffer-data :array-buffer (+ 72 48) *default-ffi-positions* :static-draw) ;TODO: :dynamic-draw?
;;     ;; positions
;;     (%gl:enable-vertex-attrib-array 0)
;;     (%gl:vertex-attrib-pointer 0 3 :float :false 0 0)
;;     ;; texture
;;     (%gl:buffer-sub-data :array-buffer 72 48 *default-ffi-tex-coordinates*)
;;     (%gl:enable-vertex-attrib-array 5)
;;     (%gl:vertex-attrib-pointer 5 2 :float :false 0 72)

;;     (gl:bind-vertex-array 0)
;;     (values vao vbo)))

