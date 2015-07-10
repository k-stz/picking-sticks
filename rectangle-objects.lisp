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


;;Sequential HASH-TABLe---------------------------------------------------------

;; why can't (class-of <hash-table>) be the superclass?
;; This provides a hash-table where the order in of the keys is specified. This is needed
;; for associating other data with the rectangles that will be stored here (e.g. textures)
(defclass sequential-hash-table ()
  ((hash-table :initform (make-hash-table) :accessor the-table)
   (keys-in-order :initform (make-array 0 :fill-pointer t) :accessor keys-in-order)))


(defun make-seq-hash-table ()
  (make-instance 'sequential-hash-table))

;; it looks like a hash-table, this might be coutner-intuitive as GETHASH, CLRHASH etc. don't work
(defmethod print-object ((s sequential-hash-table) stream)
  (declare (type stream stream))
  (let ((hash-table (the-table s)))
    (cond ((or (not *print-readably*) (not *read-eval*))
	   (print-unreadable-object (s stream :type t :identity t)
	     (format stream
		     ":TEST ~S :COUNT ~S"
		     (hash-table-test hash-table)
		     (hash-table-count hash-table)))))))


(defun clr-seq-hash (seq-hash-table)
  (setf (keys-in-order seq-hash-table) (make-array 0 :fill-pointer t))
  (clrhash (the-table seq-hash-table)))


(defmacro do-seq-hash ((var seq-hash &optional result) &body body &environment env)
  "Like DOLIST but iterates over a sequential-hash-table object."
  (declare (ignore env))
  `(let ((keys-array (keys-in-order ,seq-hash)))
     (loop for key across keys-array 
	for ,var = (gethash key (the-table ,seq-hash)) do
	  ,@body)
     ,result))


;; hash so we can querry (gethash :hero *dynamic-rectangles)
;; TODO: downside transition from hashtable to ffi-array
;;       workaround: store as array, and create a hashtable to associate name with index:
;;       (aref *dynamics-array* (gethash 'name *hash*)) ;<- abstracted away
;;       This sounds so general I bet there must be a solution out there already
;;       but this is premature optimization, MAPHASH shall suffice for now to get the values
;; CLRHASH to clear the hashtable!
(defvar *dynamic-rectangles* (make-seq-hash-table)
  "Rectangles that change often, like game objects and animations")

;; TODO: not used yet
(defvar *static-rectangles* (make-seq-hash-table)
  "Rectangles usually don't change, like background and solid scenery")

(defun print-rectangles (rectangle-hash-map)
  (let ((keys-array (keys-in-order rectangle-hash-map))
	(hash-table (the-table rectangle-hash-map)))
    ;; TODO: use :across!
    (loop for keys-index below (length keys-array) do
	 (format t "~&key:~a value:~a~%" (aref keys-array keys-index)
		 (gethash (aref keys-array keys-index) hash-table)))))

(defun add-rectangle-as (name rectangle &key (as :dynamic))
  (let* ((seq-hash-table
	 (ecase as
	   (:dynamic *dynamic-rectangles*)
	   (:static *static-rectangles*)))
	(rectangles-container (the-table seq-hash-table)))
    (multiple-value-bind (value set?) (gethash name rectangles-container)
      (declare (ignore value))
      (if set?
	(warn "The value under key: ~a was already set and has been now overwritten." name)
	(vector-push-extend name (keys-in-order seq-hash-table))))
    (setf (gethash name rectangles-container) rectangle)))



;; TODO: give nice print representation
(defclass rectangle ()
  ;; TODO: instead of vec2 provide as seperate x1-x x1-y ? So that
  ;;       transforming into 1d-array is easier (to pass into foreign-array)
  ((x1 :initarg :x1 :type vec2)
   (x2 :initarg :x2 :type vec2)
   (y1 :initarg :y1 :type vec2)
   (y2 :initarg :y2 :type vec2)))

(defun make-rectangle (&optional
			 (x 0.0)
			 (y 0.0)
			 (width 100.0)
			 (height 100.0))
  (let ((position (vec2 x y)))
    (macrolet ((vec2+ (v1 v2)
		 `(vec2 (+ (aref ,v1 0) (aref ,v2 0))
			(+ (aref ,v1 1) (aref ,v2 1)))))
      (make-instance 'rectangle
		     :x1 position
		     :x2 (vec2+ position (vec2 width 0.0))
		     :y1 (vec2+ position (vec2 0.0 height))
		     :y2 (vec2+ position (vec2 width height))))))


(defun rectangle->verts (rectangle)
  (with-slots (x1 x2 y1 y2) rectangle
    (concatenate 'vector y2 x2 x1
	  x1 y1 y2)))



(defun rectangle-seq-hash->vector (rectangle-seq-hash)
  (apply 'concatenate 'vector
	 (loop for keys-index below (keys-in-order rectangle-seq-hash)
	    collect) (rectangle->verts rectangle)))

;(defun move (rectangle))

(defvar *vao*)
(defvar *vbo*)


(defun initialize-rectangle-vao ()
  (let ((vao (first (gl:gen-vertex-arrays 1)))
	(vbo (first (gl:gen-buffers 1))))
    (gl:bind-vertex-array vao)
    ;;VBO yes, this is necessary, because it associates the vbo
    ;;with the vao
    (gl:bind-buffer :array-buffer vbo)

    (%gl:enable-vertex-attrib-array 0)
    (%gl:vertex-attrib-pointer 0 2 :float :false 0 0)

    (gl:bind-vertex-array 0)
    (gl:bind-buffer :array-buffer 0)
    (setf *vao* vao)
    (setf *vbo* vbo)))


(defun update-rectangle-vao ()
  (let* ((dynamic-verts
	  (rectangle-seq-hash->vector *dynamic-rectangles*))
	 (ffi-array (cffi:foreign-alloc :float
					:initial-contents dynamic-verts)))


    (gl:bind-vertex-array *vao*)
    (gl:bind-buffer :array-buffer *vbo*)

    
    (%gl:buffer-data :array-buffer (* 4 (length dynamic-verts)) ffi-array :static-draw)
    (cffi-sys:foreign-free ffi-array)

    (gl:bind-vertex-array 0)
    (gl:bind-buffer :array-buffer 0)))


(defun draw-rectangles ()
  (let ((dynamic-rectangle-size (* 6 (hash-table-count (the-table *dynamic-rectangles*)))))
    (%gl:draw-arrays :triangles 0 dynamic-rectangle-size)))

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

;; TODO: remove this test data
(when (< (hash-table-count (the-table *dynamic-rectangles*)) 1)
  (add-rectangle-as :hero (make-rectangle)))
