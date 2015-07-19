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
	:kit.math
	:opticl
	:opticl-utils)
  (:export :rectangle
	   ;; wow, else we can't access the unqualified from other
	   ;; packages like so (slot-value *rectangle* 'x1) ...
	   :x1 :x2 :y1 :y2
	   :make-rectangle
	   :add-rectangle-as
	   :move))

(in-package :game-objects)


;;Sequential HASH-TABLe---------------------------------------------------------

;; why can't (class-of <hash-table>) be the superclass?
;; This provides a hash-table where the order in of the keys is specified. This is needed
;; for associating other data with the rectangles that will be stored here (e.g. textures)
(defclass sequential-hash-table ()
  ((hash-table :initform (make-hash-table) :reader the-table)
   (keys-in-order :initform (make-array 0 :fill-pointer 0) :accessor keys-in-order)))


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


;; TODO: read into writing hygienic macros easily, try call-get-seq-hash approach again
(defun get-seq-hash (key seq-hash-table &optional default)
  (gethash key (the-table seq-hash-table) default))

(defun clr-seq-hash (seq-hash-table)
  (setf (keys-in-order seq-hash-table) (make-array 0 :fill-pointer t))
  (clrhash (the-table seq-hash-table)))



;; TODO:
;; > the-table/keys-in-order are also captured.. the rest are CL symbols
;; 	 so who cares
;; > assumes they are internal to the package that implements it, so also
;;   who cares
;; > the same would go for keys-array :)

;;  &environment env "only needed for macroexpand, more useful in cltl2 conforming implementation
;;  (like sbcl)
(defmacro do-seq-hash ((key value seq-hash &optional result) &body body)
  "Like DOLIST but iterates over a sequential-hash-table object."
  ;; keys-array is capturable! remove the let, sine it is only needed once
  `(call-do-seq-hash ,seq-hash (lambda (,key ,value) ,@body) ,result))


(defun call-do-seq-hash (seq-hash function result)
  (progn (loop for key across (keys-in-order seq-hash) 
	     for value = (gethash key (the-table seq-hash)) do
	       (funcall function key value))
	  result))

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
  (do-seq-hash (key value rectangle-hash-map)
    (format t "~&key:~a value:~a~%" key value)))


(defun new-print-rectangles (rectangle-hash-map)
  (do-seq-hash (rectangle key rectangle-hash-map)
    (format t "~&key:~a value:~a~%" key rectangle)))

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
  (;; postions
   (x1 :initarg :x1 :type vec2)
   (x2 :initarg :x2 :type vec2)
   (y1 :initarg :y1 :type vec2)
   (y2 :initarg :y2 :type vec2)
   ;; texture coordinates              ;; init is whole texture
                                       ;; on rectangle
   (tex-x1 :initarg :tex-x1 :type vec2 :initform (vec2 0.0 0.0))
   (tex-x2 :initarg :tex-x2 :type vec2 :initform (vec2 1.0 0.0))
   (tex-y1 :initarg :tex-y1 :type vec2 :initform (vec2 0.0 1.0))
   (tex-y2 :initarg :tex-y2 :type vec2 :initform (vec2 1.0 1.0))))

;; TODO: add texture coordinate to initilizations, providing a texture.png
;; and a fixed texture coordinate with texatl:with-sprite ?
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
    (concatenate 'vector
		 y2 x2 x1
		 x1 y1 y2)))

(defun rectangle->tex-coord (rectangle)
  (with-slots (tex-x1 tex-x2 tex-y1 tex-y2) rectangle
    (concatenate 'vector
		 tex-x1 tex-y1 tex-y2
		 tex-y2 tex-x2 tex-x1)))

(defun rectangle-seq-hash->pos-vector (rectangle-seq-hash)
  (apply 'concatenate 'vector
	 (loop for key across (keys-in-order rectangle-seq-hash)
	    collect (rectangle->verts
		     (gethash key (the-table rectangle-seq-hash))))))

(defun rectangle-seq-hash->tex-vector (rectangle-seq-hash)
  (apply 'concatenate 'vector
	 (loop for key across (keys-in-order rectangle-seq-hash)
	    collect (rectangle->tex-coord
		     (gethash key (the-table rectangle-seq-hash))))))

;(defun move (rectangle))

(defvar *vao*)
(defvar *position-vbo*)
(defvar *tex-coord-vbo*)


(defun initialize-rectangle-vao ()
  (let ((vao (first (gl:gen-vertex-arrays 1)))
	(position-vbo (first (gl:gen-buffers 1)))
	(tex-coord-vbo (first (gl:gen-buffers 1))))
    (gl:bind-vertex-array vao)
    ;;VBO yes, this is necessary, because it associates the vbo
    ;;with the vao

    ;;postion
    (gl:bind-buffer :array-buffer position-vbo)
    (%gl:enable-vertex-attrib-array 0)
    ;; TODO: size 3 for depth?
    (%gl:vertex-attrib-pointer 0 2 :float :false 0 0)

    ;;texCoord
    (gl:bind-buffer :array-buffer tex-coord-vbo)
    (%gl:enable-vertex-attrib-array 1)
    (%gl:vertex-attrib-pointer 1 2 :float :false 0 0)
    

    (gl:bind-vertex-array 0)
    (gl:bind-buffer :array-buffer 0)
    (setf *vao* vao)
    (setf *position-vbo* position-vbo
	  *tex-coord-vbo* tex-coord-vbo)))


(defun update-rectangle-vao ()
  (let* ((dynamic-verts
	  (rectangle-seq-hash->pos-vector *dynamic-rectangles*))
	 (pos-ffi-array (cffi:foreign-alloc :float
					    :initial-contents dynamic-verts))
	 (tex-coordinates
	  (rectangle-seq-hash->tex-vector *dynamic-rectangles*))
	 (tex-ffi-array (cffi:foreign-alloc :float
					    :initial-contents tex-coordinates)))

    (gl:bind-vertex-array *vao*)

    ;;position
    (gl:bind-buffer :array-buffer *position-vbo*)
    ;; size-of(flaot) => 4, hence we multiply with 4
    (%gl:buffer-data :array-buffer (* 4 (length dynamic-verts)) pos-ffi-array :static-draw)


    ;;texture coordinates
    (gl:bind-buffer :array-buffer *tex-coord-vbo*)
    (%gl:buffer-data :array-buffer (* 4 (length tex-coordinates)) tex-ffi-array :static-draw)

    ;;free foreign data
    (cffi-sys:foreign-free pos-ffi-array)
    (cffi-sys:foreign-free tex-ffi-array)

    
    (gl:bind-vertex-array 0)
    (gl:bind-buffer :array-buffer 0)))


(defun draw-rectangles ()
  (let ((dynamic-rectangle-size (* 6 (hash-table-count (the-table *dynamic-rectangles*)))))
    ;; TODO: :triangle-fan could be much easier, will texture coordinate mappinng work
    ;; straightforwardly too?
    (%gl:draw-arrays :triangles 0 dynamic-rectangle-size)))

(defvar *default-ffi-positions*
  (cffi:foreign-alloc :float :initial-contents
		      #(0.5 0.5 0.5 0.5 -0.5 0.5 -0.5 -0.5 0.5
			-0.5 -0.5 0.5 -0.5 0.5 0.5 0.5 0.5 0.5)))

(defvar *default-ffi-tex-coordinates*
  (cffi:foreign-alloc :float :initial-contents
		      #(1.0 0.0   1.0 1.0   0.0 1.0
			0.0 1.0   0.0 0.0   1.0 0.0)))

;; TODO: remove this test data
(when (< (hash-table-count (the-table *dynamic-rectangles*)) 1)
  (add-rectangle-as :hero (make-rectangle)))

;;Texture-----------------------------------------------------------------------

;; TODO: integrate "texatl"
(defvar *rectangle-texture*)
(defvar *rectangle-sampler*)

;; arbitrarily 10, so we don't collide with anything for now
;; iirc up to 80 are guaranteed
(defvar *tex-unit* 10)

(defun create-rectangle-texture ()
  (setf *rectangle-texture* (first (gl:gen-textures 1)))

  (%gl:bind-texture :texture-2d *rectangle-texture*)
  ;; TODO: understand
  (gl:tex-parameter :texture-2d :texture-base-level 0)
  (gl:tex-parameter :texture-2d :texture-max-level 0)


  (setf *rectangle-sampler* (first (gl:gen-samplers 1)))
  ;; TODO understand
  (gl:sampler-parameter *rectangle-sampler* :texture-mag-filter :nearest)
  (gl:sampler-parameter *rectangle-sampler* :texture-min-filter :nearest)
  (gl:sampler-parameter *rectangle-sampler* :texture-wrap-s :repeat) ;; change to repeat
  (gl:sampler-parameter *rectangle-sampler* :texture-wrap-t :repeat))


(defvar *foo-img-object* (opticl-utils:image-file->image-object "foo.png"))
(defvar *nyo-png* (opticl-utils:image-file->image-object "resources/nyo.png"))

(defvar *global-texture* *nyo-png*)

(defun update-rectangle-texture ()
  ;; make *tex-unit* the _current_ texture image unit, opengl 
  (%gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) *tex-unit*))
  (%gl:bind-texture :texture-2d *rectangle-texture*)
  (%gl:bind-sampler *tex-unit* *rectangle-sampler*)
  ;; TODO: each iteration or just on creation
  (gl:sampler-parameter *rectangle-sampler* :texture-mag-filter :nearest)
  (gl:sampler-parameter *rectangle-sampler* :texture-min-filter :linear)

  ;; already done in game.lisp init code, TODO: need to change architecture here
  ;; (uniform :int :test-texture *tex-unit-1*)
  (with-slots (width height pixels pos-ffi-array) *global-texture*
    (gl:tex-image-2d :texture-2d 0 :rgba8
		     width
		     height 0
		     :rgba		;components per element
		     :unsigned-byte	;; normalized integer
		     pos-ffi-array)))

;;Transformations---------------------------------------------------------------

;; we can only move *dynamic-rectangles*
(defun move-rectangle (rectangle direction-vec2)
  (assert (typep direction-vec2 '(simple-array single-float (2))))
  (macrolet ((vec2+ (v1 v2)
	       `(vec2 (+ (aref ,v1 0) (aref ,v2 0))
		      (+ (aref ,v1 1) (aref ,v2 1)))))
    (with-slots (x1 x2 y1 y2) rectangle
      (setf x1 (vec2+ x1 direction-vec2))
      (setf x2 (vec2+ x2 direction-vec2))
      (setf y1 (vec2+ y1 direction-vec2))
      (setf y2 (vec2+ y2 direction-vec2)))))

(defun move (name direction-vec2)
  ;; for now we assume only *dynamic-rectangles* can be moved
  (let ((rectangle (gethash name (the-table *dynamic-rectangles*))))
    (move-rectangle rectangle direction-vec2)))

;;Animation---------------------------------------------------------------------

(defclass animation ()
  ((start-time :initarg :start-time)
   (spritesheet :type TEXATL.CL:TEXATL-SPRITESHEET)
   (frames :type integer)
   ;; idea is to pass those as arguments like so:
   ;; (texatl.cl:sprite <spritesheet> '(<sprite-name> <mode> <direction>) 0)
   ;; though it forces a particular kind of format for our spritesheet
   (sprite-name :type keyword)
   (mode :type keyword)
   (direction :type keyword)
   (up-animation :type keyword)))

(defmethod animate ((a animation) )
  a)
