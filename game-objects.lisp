;; Idea: one array of static rectangles (background)
;;       and one array of constantly changing rectangles
;;      => write all into one vbo (gl:buffer-data ...)
;;      - draws all with one call
;;      - much better performance, less overhead with individual
;;      - gl:buffer-sub-data 
;;
;;      => change rectangle data (position,color) on cpu/cl side
;;         using simple arrays

;; Possible problem: what if a rectangle disappears? How to remove it from
;; the sequential hash-table?

;; TODO: texatl to get sprites and spritesheet integretation, maybe even text!!

;; one big texture to read all the data from, again using texatl?



(defpackage :game-objects
  (:use :cl
	;; oooooh, only the EXTERNAL symbols get inherited!!
	:kit.gl.shader
	:kit.math
	:opticl
	:opticl-utils
	:math)
  (:export :rectangle
	   ;; wow, else we can't access the unqualified from other
	   ;; packages like so (slot-value *rectangle* 'x1) ...
	   :x1 :x2 :y1 :y2 :center-point :radius :bounding-volume
	   ;; BV
	   :set-radius
	   ;; rectangle
	   :make-rectangle
	   :make-rectangle-c
	   :add-rectangle-as
	   :remove-rectangle
	   :get-rectangle
	   :move
	   :scale
	   :rotate
	   ;;utils
	   :do-seq-hash
	   ;;animation
	   :set-animation
	   :next-animation-frame))

(in-package :game-objects)


;;Sequential HASH-TABLE---------------------------------------------------------

;; why can't (class-of <hash-table>) be the superclass?
;; This provides a hash-table where the order of the keys is specified. This is needed
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

(defun get-rectangle (name &optional (rectangle-set *dynamic-rectangles*))
  (get-seq-hash name rectangle-set))

(defun clr-seq-hash (seq-hash-table)
  (setf (keys-in-order seq-hash-table) (make-array 0 :fill-pointer t))
  (clrhash (the-table seq-hash-table)))

(defun rem-seq-hash (key seq-hash-table)
  (with-slots (keys-in-order) seq-hash-table
    (let* ((new-ordered-keys
	    (remove-if (lambda (x) (equal x key))
		       keys-in-order))
	   (length (length new-ordered-keys)))
      (setf keys-in-order
	    (make-array length
			:initial-contents new-ordered-keys
			:fill-pointer length))))
  (remhash key (the-table seq-hash-table)))


(defun remove-rectangle (name &optional (seq-hash-table *dynamic-rectangles*))
  (rem-seq-hash name seq-hash-table))


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
  (progn (maphash function (the-table seq-hash))
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

(defun add-rectangle-as (name rectangle &key (as :dynamic) to)
  (let* ((seq-hash-table
	  (if to
	      to
	      (ecase as
		(:dynamic *dynamic-rectangles*)
		(:static *static-rectangles*))))
	 (rectangles-container (the-table seq-hash-table)))
    (multiple-value-bind (value set?) (gethash name rectangles-container)
      (declare (ignore value))
      (if set?
	  (warn "The value under key: ~a was already set and has been now overwritten." name)
	  (vector-push-extend name (keys-in-order seq-hash-table))))
    (setf (gethash name rectangles-container) rectangle)))


;;Rectangle---------------------------------------------------------------------

(defclass animation ()
  ((start-time :initarg :start-time :initform 0)
   ;; used to determine how many times the animation state has been querried for
   ;; an animation frame, to determine how to change the state. This effectively
   ;; in what order the animation going to be showsn
   (animation-querries  :type integer :initform 0)
   ;; this determines how many aimation-querries are needed
   (querries-per-frame :type integer)
   ;; hm this is the global-texture, so there should also be a global sprite sheet
   ;; (spritesheet :type TEXATL.CL:TEXATL-SPRITESHEET :initarg :spritesheet)
   (frame :type integer :initform 0)
   ;; how many distinct frames are available for the aimation to cycle through
   (frame-count :type integer :initform 3 :reader frame-count)
   ;; idea is to pass those as arguments like so:
   ;; (texatl.cl:sprite <spritesheet> '(<sprite-name> <mode> <direction>) 0)
   ;; though it forces a particular kind of format for our spritesheet
   (sprite-name :type keyword :initarg :sprite-name :initform :default-name)
   (mode :type keyword :initform :walk)
   (direction :type keyword :initform :down)
   (default-animation :type keyword :initform :walk)))

(defmethod :after initialize-instance ((animation animation) &key)
  )


(defclass collision-rectangle ()
  ((center-point :initarg :center-point :type vec3)
   (radius :initarg :radius :type vec3)
   ;; stores current rotation. Used for quick
   ;; AABB recalculation after rotation
   (radians-vec3 :initform (vec3 0.0 0.0 0.0) :type vec3)))

;; TODO: should be useless now
;; (defgeneric incf-radians (rectangle delta))
;; (defmethod incf-radians ((rectangle collision-rectangle) delta-vec3)
;;   (with-slots (radians-vec3) rectangle
;;     ;; "keeping the hands of the clock inside the clock" -- Mod
;;     (macrolet ((r (subscript)
;; 		 `(aref radians-vec3 ,subscript)))
;;       (macrolet ((force-inside-clock (delta-subscript)
;; 		   `(setf (r ,delta-subscript)
;; 			  (coerce (mod (+ (r ,delta-subscript)
;; 					  (aref delta-vec3 ,delta-subscript))
;; 				       (* 2 pi))
;; 				  'single-float))))
;; 	(force-inside-clock 0)
;; 	(force-inside-clock 1)
;; 	(force-inside-clock 2)
;; 	radians-vec3))))


(defgeneric set-radius (collision-rectangle radius)
  (:method ((rectangle collision-rectangle) radius-vec3)
    (setf (slot-value rectangle 'radius)
    	  radius-vec3)))

;; TODO: give useful print representation
;; TODO: make super-class rectangle and sub-class game-object. Seperating rendering
;;       representation from interior representation and proxy geometry for collision
;;       tests
;; UPDATE: maybe go with the superclass: rectangle, subclass: game-object approach, which
;;         will contain a representation slot (e.g. filled with sphere, rectangle), an
;;         <animation> slot a proxy geometry, a cached geometry (past timesteam when no
;;         collision orientation)
(defclass rectangle ()
  ;; TODO: instead of vec2 provide as seperate x1-x x1-y ? So that
  ;;       transforming into 1d-array is easier (to pass into foreign-array)
  (;; postions
   ;; TODO: start counting at 0 ?
   (x1 :initarg :x1 :type vec3)
   (x2 :initarg :x2 :type vec3)
   (y1 :initarg :y1 :type vec3)
   (y2 :initarg :y2 :type vec3)

   ;; center-radius representation
   (center-point :initarg :center-point :type vec3)
   ;; {rx, ry, rz} rz is meaningless for 2d-rectangles, the "depth" attribute
   ;; is the z-component of the center-point
   (radius :initarg :radius :type vec3)

   ;; texture coordinates              ;; init is whole texture
                                       ;; on rectangle
   (tex-x1 :initarg :tex-x1 :type vec2 :initform (vec2 0.0 0.0))
   (tex-x2 :initarg :tex-x2 :type vec2 :initform (vec2 1.0 0.0))
   (tex-y1 :initarg :tex-y1 :type vec2 :initform (vec2 0.0 1.0))
   (tex-y2 :initarg :tex-y2 :type vec2 :initform (vec2 1.0 1.0))

   ;; Bounding Volume used for collision test - the proxy geometry
   ;; Now all transformation functions must also update this one
   (bounding-volume :initarg :bounding-volume :type collision-rectangle
		    :accessor bounding-volume)

   ;; for now we directly couple animation with the rectangle
   (animation-state :type animation :initform (make-animation) :reader animation-state)))


;; rectangle print representation
(defmethod print-object ((rectangle rectangle) stream)
  (declare (type stream stream))
  (with-slots (center-point radius) rectangle
    (when (or (not *print-readably*) (not *read-eval*))
      (print-unreadable-object (rectangle stream :type t :identity t)
	(format stream
		"center-point:~S radius:~S"
		center-point radius)))))


;; Bounding Volume calculation
(defgeneric translate-bounding-volume (bounding-volume vec3))
(defgeneric scale-bounding-volume (bounding-volume vec3))
(defgeneric rotate-bounding-volume (bounding-volume vec3))


(defmethod translate-bounding-volume ((collision-rectangle collision-rectangle) direction-vec3)
  (with-slots (center-point) collision-rectangle
    (setf center-point (vec3+ center-point direction-vec3))))

(defmethod scale-bounding-volume ((collision-rectangle collision-rectangle) scale-vec3)
  (with-slots (radius) collision-rectangle
    (macrolet ((r (subscript)
		 `(aref radius ,subscript))
	       (s (subscript)
		 `(aref scale-vec3 ,subscript)))
      ;; TODO: hm (vec3* ) allocates a fresh vector, while this directly changes radius'
      ;; subscripts.
      (setf (r 0) (* (r 0) (s 0)))
      (setf (r 1) (* (r 1) (s 1)))
      (setf (r 2) (* (r 2) (s 2))))))



(defgeneric update-aabb (game-object transformation-matrix translation-vector))

;; ugh, this only works if we assume a base radius, because it draws a fresh aabb around
;; it, drawing a aabb around the old grows the aabb to infinity. But we can't have
;; a base radius since we want to do arbitrary scaling as well. Rather, we will
;; go straight for OBB (oriented bounding boxes) which probably use a rotation matrix
;; and a translation
(defmethod update-aabb ((collision-rectangle collision-rectangle) mat4 translation-vec3)
  (let ((new-center-point (vec3 0.0))
	(new-radius (vec3 0.0)))
    (with-slots (center-point radius radians-vec3) collision-rectangle
      (macrolet ((c (subscript) `(aref new-center-point ,subscript))
		 (r (subscript) `(aref new-radius ,subscript))
		 (t (subscript) `(aref translation-vec3 ,subscript)))
	(loop for i from 0 below 3 do
	     (setf (c i) (t i))
	     (setf (r i) 0.0)
	     (loop for j from 0 below 3 do
	     	  (incf (c i) (* (mat4-place mat4 i j) (aref center-point j)))
		  (incf (r i) (* (abs (mat4-place mat4 i j)) (aref radius j))))))
      (format t "~%")
      (print radius)
      (print new-radius)
      collision-rectangle)))


;; TODO: doesn't work properly, needs base rectangle data or it keeps on adding a bigger
;;       AABB around the old smaller one growing it indefinetely. You get the idea, just
;;       abandon this and move on to OBB?
(defmethod rotate-bounding-volume ((collision-rectangle collision-rectangle) rotation-vec3)
  (with-slots (radius radians-vec3) collision-rectangle
    
    (update-aabb collision-rectangle (sb-cga:rotate radians-vec3)
		 (vec3 0.0 0.0 0.0))))





;; TODO: obsolete, remove!
(defgeneric recalculate-aabb-radius (game-object))

;; this assumes that the CENTER-POINT is correct
;; example use: new aabb after rotation
(defmethod recalculate-aabb-radius ((rectangle rectangle))
  (with-slots (x1 x2 y1 y2 radius center-point) rectangle
    (macrolet ((r (subscript)
		 `(aref radius ,subscript)))
      ;; x-axis radius
      (multiple-value-bind (min-along-x max-along-x)
	  (collision:extreme-points-along-direction
	   (vec3 1.0 0.0 0.0)
	   (list x1 x2 y1 y2))
	;; max-min = bv-x-diameter
	;; (/ bv-diameter 2.0) = bv-x-radius
	(setf
	 (r 0)
	 (/
	  (aref (vec3- max-along-x min-along-x) 0)
	  2.0)))
      ;; y-axis radius
      (multiple-value-bind (min-along-y max-along-y)
	  (collision:extreme-points-along-direction
	   (vec3 0.0 1.0 0.0)
	   (list x1 x2 y1 y2))
	;; max-min = bv-y-diameter
	;; (/ bv-diameter 2.0) = bv-y-radius
	(setf
	 (r 1)
	 (/
	  (aref (vec3- max-along-y min-along-y) 1)
	  2.0))))
    (with-slots ((bv-center-point center-point)) (bounding-volume rectangle)
      (setf bv-center-point center-point))))


;;

(defun center-radius->vertices (center-point-vec3 radius-vec3)
  "Returns the points of the 2d-rectangle of the center-radius representation given.
Note the z-component of the radius-vec3 will decide the depth of all the points (along the z-axis)!"
  (let* ((rx (aref radius-vec3 0))
	 (ry (aref radius-vec3 1))
	 (rz (aref radius-vec3 2))
	 (x1 (vec3- center-point-vec3 (vec3 rx ry (- rz))))
	 (x2 (vec3+ center-point-vec3 (vec3 rx (- ry) rz)))
	 (y1 (vec3+ center-point-vec3 (vec3 (- rx) ry rz)))
	 (y2 (vec3+ center-point-vec3 (vec3 rx ry rz))))
    (values x1 x2 y1 y2)))

;; TODO: add texture coordinate to initilizations, providing a texture.png
;; and a fixed texture coordinate with texatl:with-sprite ?
;; UPDATE: maybe go with the superclass: rectangle, subclass: game-object which will contain
;;         a representation point (e.g. filled with sphere, rectangle), an <animation> slot
;;         a proxy geometry, a cached geometry (past timesteam when no collision orientation)
(defun make-rectangle-c (&optional
			   (center-point (vec3 0.0 0.0 0.0))
			   (radius (vec3 50.0 50.0 0.0)))
  "Make a rectangle using the center-radius representation. It uses less storage
is more efficient in aabb collision tests!"
  (multiple-value-bind (x1 x2 y1 y2)
      (center-radius->vertices center-point radius)
    (make-instance 'rectangle
		   :center-point center-point
		   :radius radius
		   :x1 x1
		   :x2 x2
		   :y1 y1
		   :y2 y2
		   ;; default bounding volume is an AABB rectangle provide with
		   ;; the center-radius representation
		   :bounding-volume (make-instance 'collision-rectangle
						   :radius radius
						   :center-point center-point))))

(defun make-rectangle (&optional
			 (x 0.0)
			 (y 0.0)
			 (width 100.0)
			 (height 100.0)
			 (depth 0.0))
  (let* ((rx (* width 0.5))
	 (ry (* height 0.5))
	 (rz depth)
	 (radius (vec3 rx ry rz))
	 (center-point (vec3 (+ x rx)
			     (+ y ry)
			     depth)))
    (make-rectangle-c center-point radius)))




(defun get-position (rectangle-name &optional (seq-hash-table *dynamic-rectangles*))
  (slot-value (get-rectangle rectangle-name seq-hash-table) 'x1))

(defun set-rectangle-depth (rectangle depth)
  (setf (aref (slot-value rectangle 'x1) 2) depth)
  (setf (aref (slot-value rectangle 'x2) 2) depth)
  (setf (aref (slot-value rectangle 'y1) 2) depth)
  (setf (aref (slot-value rectangle 'y2) 2) depth))

(defun rectangle->verts (rectangle)
  (with-slots (x1 x2 y1 y2) rectangle
    (concatenate 'vector
		 y2 x2 x1
		 x1 y1 y2)))

(defun rectangle->tex-coord (rectangle)
  (with-slots (tex-x1 tex-x2 tex-y1 tex-y2) rectangle
    (concatenate 'vector
		 tex-x2 tex-y2 tex-y1
		 tex-y1 tex-x1 tex-x2)))

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
    (%gl:vertex-attrib-pointer 0 3 :float :false 0 0)

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
	 (static-verts
	  (rectangle-seq-hash->pos-vector *static-rectangles*))
	 (verts (concatenate 'vector
			     dynamic-verts
			     static-verts))
	 (pos-ffi-array (cffi:foreign-alloc :float
					    :initial-contents
					    verts))
	 (dynamic-tex-coordinates
	  (rectangle-seq-hash->tex-vector *dynamic-rectangles*))
	 (static-tex-coordinates
	  (rectangle-seq-hash->tex-vector *static-rectangles*))
	 (tex-coordinates (concatenate 'vector
				       dynamic-tex-coordinates
				       static-tex-coordinates))
	 (tex-ffi-array (cffi:foreign-alloc :float
					    :initial-contents
					    tex-coordinates)))

    (gl:bind-vertex-array *vao*)

    ;;position
    (gl:bind-buffer :array-buffer *position-vbo*)
    ;; size-of(flaot) => 4, hence we multiply with 4
    (%gl:buffer-data :array-buffer (* 4 (length verts)) pos-ffi-array :static-draw)


    ;;texture coordinates
    (gl:bind-buffer :array-buffer *tex-coord-vbo*)
    (%gl:buffer-data :array-buffer (* 4 (length tex-coordinates)) tex-ffi-array :static-draw)

    ;;free foreign data
    (cffi-sys:foreign-free pos-ffi-array)
    (cffi-sys:foreign-free tex-ffi-array)

    (gl:bind-vertex-array 0)
    (gl:bind-buffer :array-buffer 0)))


;; It might be better to decouple *dynamic-rectangle* from this and rather create use a list
;; of seq-hash-tables? ;;<- NEXT-TODO, good idea
(defun draw-rectangles ()
;;  (declare (optimize debug))
  (let ((dynamic-rectangle-size (* 6 (+ (hash-table-count (the-table *dynamic-rectangles*))
					(hash-table-count (the-table *static-rectangles*))))))
    ;; TODO: :triangle-fan could be much easier, will texture coordinate mapping work
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

(defvar *global-spritesheet* tex::*nyo-spritesheet*)

(defun update-rectangle-texture ()
  ;; make *tex-unit* the _current_ texture image unit, opengl 
  (%gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) *tex-unit*))
  (%gl:bind-texture :texture-2d *rectangle-texture*)
  (%gl:bind-sampler *tex-unit* *rectangle-sampler*)
  ;; TODO: each iteration or just on creation
  (gl:sampler-parameter *rectangle-sampler* :texture-mag-filter :nearest)
  (gl:sampler-parameter *rectangle-sampler* :texture-min-filter :nearest)

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

(defun move-rectangle (rectangle direction-vec2-or-vec3)
  (let ((direction-vec3
	 (cond ((typep direction-vec2-or-vec3 '(SIMPLE-ARRAY SINGLE-FLOAT (2)))
		(vec3 (aref direction-vec2-or-vec3 0)
		      (aref direction-vec2-or-vec3 1)
		      0.0))
	       ((typep direction-vec2-or-vec3 '(SIMPLE-ARRAY SINGLE-FLOAT (3)))
		direction-vec2-or-vec3))))
    (with-slots (x1 x2 y1 y2 center-point) rectangle
      (setf x1 (vec3+ x1 direction-vec3))
      (setf x2 (vec3+ x2 direction-vec3))
      (setf y1 (vec3+ y1 direction-vec3))
      (setf y2 (vec3+ y2 direction-vec3))
      ;; central-radius representation
      (setf center-point (vec3+ center-point direction-vec3)))
    (translate-bounding-volume (bounding-volume rectangle) direction-vec3)))

(defun move (name direction-vec2 &optional (seq-hash-table *dynamic-rectangles*))
  ;; for now we assume only *dynamic-rectangles* can be moved
  (let ((rectangle (gethash name (the-table seq-hash-table))))
    (move-rectangle rectangle direction-vec2)))


(defun move-to (name point &optional (seq-hash-table *dynamic-rectangles*))
  (let ((rectangle (get-rectangle name seq-hash-table)))
    (with-slots (x1 x2 y1 y2 radius center-point) rectangle
      ;; this provides the following convenience:
      ;; vec2 input -> default z value to current depth
      ;; vec3 input -> new center-point of rectangle
      (setf point
	    (cond ((typep point 'vec2)
		   (vec3 (aref point 0)
			 (aref point 1)
			 ;; depth value, the default 3 component if vec2 is provided
			 (aref radius 2)))
		  ((typep point 'vec3)
		   point)))
      ;; translate bounding volume:
      (translate-bounding-volume (bounding-volume rectangle)
				 (vec3- point center-point))
      ;; update rendering data:
      (multiple-value-bind (n-x1 n-x2 n-y1 n-y2)
	  (center-radius->vertices point radius)
	(setf center-point point
	      x1 n-x1
	      x2 n-x2
	      y1 n-y1
	      y2 n-y2)))))


;; TODO: remove after tests:
(defun matrix->translate-vec3 (matrix)
  (vec3 (aref matrix 12)
	(aref matrix 13)
	(aref matrix 14)))
;; /TESTs

(defun scale-rectangle (rectangle scale-vec3)
  ;; TODO: handle 0.0-factor scaling
  (with-slots (x1 x2 y1 y2 radius center-point) rectangle
    (let* ((x1->y1 (vec3- y1 x1))	   
	   (up-dir (sb-cga:normalize x1->y1))
	   (z-axis-rotation (acos (round-dot
				   ;; NEXT-TODO DOT-PRODUCT is the culprit!
				   ;; rotation beyond 180-degree doesn't work
				   ;; with this properly!
				   ;; SOLUTION: (IF (CROSS-PRODCUCT ...)
				                    ;; <this was around>
						    ;; <other way around>)
				   ;; add tighter BV to nyo initialization for
				   ;; more intuitive collision testing
				   (sb-cga:dot-product (vec3 0.0 1.0 0.0)
						       up-dir))))
	   (into-origin-translation (sb-cga:translate (vec3- center-point)))
	   (from-origin-translation (sb-cga:translate center-point))
	   (scale-matrix (sb-cga:scale scale-vec3))
	   (rotation-matrix (sb-cga:rotate (vec3 0.0 0.0 z-axis-rotation)))
	   (rotation-matrix-1 (sb-cga:rotate (vec3 0.0 0.0 (- z-axis-rotation))))
	   ;; (rotation-matrix-1 (sb-cga:inverse-matrix rotation-matrix))
	   
	   (transformation-matrix
	    (sb-cga:matrix*
	     from-origin-translation
	     rotation-matrix
	     scale-matrix
	     rotation-matrix-1
	     into-origin-translation)))
;      (print z-axis-rotation)
      (print radius)
      ;; (print rotation-matrix)
      ;; TODO: non-uniform scaling fails because of mat4*vec3 ??
      (setf x1 (matrix->translate-vec3 (sb-cga:matrix* transformation-matrix (sb-cga:translate x1))))
      (setf x2 (matrix->translate-vec3 (sb-cga:matrix* transformation-matrix (sb-cga:translate x2))))
      (setf y1 (matrix->translate-vec3 (sb-cga:matrix* transformation-matrix (sb-cga:translate y1))))
      (setf y2 (matrix->translate-vec3 (sb-cga:matrix* transformation-matrix (sb-cga:translate y2))))

      ;; (setf x1 (mat4*vec3 transformation-matrix x1))
      ;; (setf x2 (mat4*vec3 transformation-matrix x2))
      ;; (setf y1 (mat4*vec3 transformation-matrix y1))
      ;; (setf y2 (mat4*vec3 transformation-matrix y2))
      )
    (recalculate-aabb-radius rectangle)))

(defun scale (name factor &optional (seq-hash-table *dynamic-rectangles*))
  (let ((rectangle (get-rectangle name seq-hash-table)))
    (scale-rectangle rectangle (vec3 factor factor factor))))


(defun rotate-rectangle (rectangle rotation-vec3 &optional around-vec3)
  (with-slots (x1 x2 y1 y2 center-point) rectangle
    (let* ((origin (if around-vec3
		       around-vec3
		       center-point))
	   (into-origin-translation (sb-cga:translate (vec3- origin)))
	   (from-origin-translation (sb-cga:translate origin))
	   (rotation-matrix (sb-cga:rotate rotation-vec3))
	   (transformation-matrix
	    (sb-cga:matrix*
	     from-origin-translation
	     rotation-matrix
	     into-origin-translation)))
      (setf x1 (mat4*vec3 transformation-matrix x1))
      (setf x2 (mat4*vec3 transformation-matrix x2))
      (setf y1 (mat4*vec3 transformation-matrix y1))
      (setf y2 (mat4*vec3 transformation-matrix y2))

      (when around-vec3
	(setf center-point (mat4*vec3 transformation-matrix center-point)))
      (rotate-bounding-volume (bounding-volume rectangle)
      			      rotation-vec3))))


(defun rotate (name degree &optional (seq-hash-table *dynamic-rectangles*))
  (let ((radians (deg-to-rad degree))
	(rectangle (get-rectangle name seq-hash-table)))
    (rotate-rectangle rectangle (vec3 0.0 0.0 radians))))

;;;Animation


(defun make-animation ()
  (make-instance 'animation))

;; TODO: operates only on rectangles in *dynamic-rectangles* due to GET-RECTANGLE, better
;; design needed here
(defun change-animation-state (rectangle new-mode new-direction &optional new-frame new-sprite-name)
  (let ((animation-state (animation-state rectangle)))
    (with-slots (sprite-name mode direction frame frame-count) animation-state
      (setf mode new-mode
	    direction new-direction)
      (when new-sprite-name
	(setf sprite-name new-sprite-name))
      (when new-frame
	(setf frame new-frame))
      (setf frame-count
	    (texatl.cl:frame-count *global-spritesheet*
				   (list sprite-name mode direction))))))

(defun set-animation (name mode direction &optional frame sprite-name)
  (let ((rectangle (get-rectangle name)))
    (change-animation-state rectangle mode direction frame sprite-name)
    (apply-animation-state rectangle)))

(defgeneric apply-animation-state (rectangle))
(defmethod apply-animation-state ((rectangle rectangle))
  (let ((animation-state (animation-state rectangle))
	(width (slot-value *global-texture* 'width))
	(height (slot-value *global-texture* 'height)))
    (with-slots (sprite-name mode direction frame) animation-state
     ; (print (list sprite-name mode direction frame))
      (texatl.cl:with-sprite (x0 y0 x1 y1)
	  (list sprite-name mode direction) frame *global-spritesheet*
	(let ((x0 (/ x0 width))
	      (y0 (/ y0 height))
	      (x1 (/ x1 width))
	      (y1 (/ y1 height)))
	  (with-slots (tex-x1 tex-x2 tex-y1 tex-y2) rectangle
	    ;; oh wow, the rectangle representation of the texatl system
	    ;; might have some efficiency advantages, the disadvantage is that
	    ;; it doesn't allow us to represent rectangles that aren't axis aligned
	    (setf tex-x1 (vec2 x0 y0))
	    (setf tex-x2 (vec2 x1 y0))
    	    (setf tex-y1 (vec2 x0 y1))
	    (setf tex-y2 (vec2 x1 y1))

	    (list x0 y0 x1 y1 (list tex-x1 tex-x2 tex-y1 tex-y2))))))))

(defmethod animate ((rectangle rectangle))
  (when (slot-boundp (slot-value rectangle 'animation-state) 'spritesheet)
    (apply-animation-state rectangle)))

(defmethod next-rectangle-animation-frame ((rectangle rectangle))
  (let* ((animation (animation-state rectangle))
	 (max-frames (frame-count animation))
	 (frame (slot-value animation 'frame)))
    (if (>= frame (1- max-frames))
	(setf (slot-value animation 'frame) 0)
	(incf (slot-value animation 'frame)))
    (apply-animation-state rectangle)))

(defun next-animation-frame (rectangle-name)
  "Advances the frame (picture) on the rectangle to the next, based on the
animation state of the object."
  (let ((rectangle (get-rectangle rectangle-name)))
    (next-rectangle-animation-frame rectangle)))


(defun print-animation-state (rectangle-name)
  (let ((animation-state (slot-value (get-rectangle rectangle-name) 'animation-state)))
    (with-slots (start-time frame sprite-name mode direction default-animation)
	animation-state
      ;; TODO: make a neat table
      (format t "~&start-time:~a ~&frame:~a ~&sprite:~a ~&mode:~a ~&direction:~a ~&default:~a~%"
	      start-time frame sprite-name mode direction default-animation))))

;; TODO: delete
(defun tex (rectangle)
  (with-slots (tex-x1 tex-x2 tex-y1 tex-y2) rectangle
    (list tex-x1 tex-x2 tex-y1 tex-y2)))

;;Test data---------------------------------------------------------------------

;; TODO: remove this test data
(when (< (hash-table-count (the-table *dynamic-rectangles*)) 1)
  (add-rectangle-as :hero (make-rectangle)))

