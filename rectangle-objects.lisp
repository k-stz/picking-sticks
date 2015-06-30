
(defpackage :game-objects
  (:use :cl
	;; oooooh, only the EXTERNAL symbols get inherited!!
	:game))

(in-package :game-objects)


(defclass image-object ()
  ((height :initarg :height)
   (width :initarg :width)
   (pixels :initarg :pixels)
   (ffi-array)))

;; try to collect everything needed to draw arbitrary rectangles
(defclass rectangle-object ()
  ((positions :initarg :positions)
   (texture-coordinates :initform
			#(1.0 0.0   1.0 1.0   0.0 1.0   0.0 1.0   0.0 0.0   1.0 0.0))
   (image-data :initarg :image-data :type 'image-object)
   ;; vao
   (vao)
   (vbo)
   ))


(defvar *default-ffi-positions*
  (cffi:foreign-alloc :float :initial-contents
		      #(0.5 0.5 0.5 0.5 -0.5 0.5 -0.5 -0.5 0.5
			-0.5 -0.5 0.5 -0.5 0.5 0.5 0.5 0.5 0.5)))

(defvar *default-ffi-tex-coordinates*
  (cffi:foreign-alloc :float :initial-contents
		      #(1.0 0.0   1.0 1.0   0.0 1.0
			0.0 1.0   0.0 0.0   1.0 0.0)))

;; TODO: allow to make rectangle-objects without gl-context? Maybe add function
;; to build rectangle-object "on-gpu" I think that's how CEPL's gpu-* functions
;; are named like that
(defun make-gpu-rectangle ()
  "Returns VAO and VBO for a GPU rectangle data with vbo containing position and texture
Data"
  ;; TODO: if works, work on inder-buffer-object
  (let ((vao (first (gl:gen-vertex-arrays 1)))
	(vbo (first (gl:gen-buffers 1))))
    (gl:bind-buffer :array-buffer vbo)
    ;; Positions:
    ;; 2 triangles * 3 vertices * 3 components xyz * of 4 bytes (float) = 72
    ;; textures:
    ;; 2 triangles * 3 vertices * 2 components * of 4 bytes (float) = 48
    (%gl:buffer-data :array-buffer (+ 72 48) *default-ffi-positions* :static-draw) ;TODO: :dynamic-draw?
    ;; positions
    (%gl:enable-vertex-attrib-array 0)
    (%gl:vertex-attrib-pointer 0 3 :float :false 0 0)
    ;; texture
    (%gl:buffer-sub-data :array-buffer 72 48 *default-ffi-tex-coordinates*)
    (%gl:enable-vertex-attrib-array 5)
    (%gl:vertex-attrib-pointer 5 2 :float :false 0 72)

    (gl:bind-vertex-array 0)
    (values vao vbo)))


(defparameter img (make-instance 'image-object :height 10))
(defparameter foo (make-instance 'rectangle-object :image-data img))
