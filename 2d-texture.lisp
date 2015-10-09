;; sbcl 2.6% mem

(defpackage :2d-texture
  ;; :use inherits all the exported symbols from the package given
  (:use :cl
	;; this one's important, as all the defclass lambda lists
	;; and the the implicitly created generic functions
	;; appear in the example unqualified, as they're interned
	:sdl2.kit
	:kit.gl.shader
	:kit.math
	:opticl))

(in-package :2d-texture)

;; with this you can call gl cmds in the repl while the sdl2kit window is running
;; affecting it:
;; repl> #m (gl:sampler-parameter *sampler* :texture-wrap-t :mirror-clamp-to-edge)
;; TODO: where to put reader-macro definitions. Surround by (evel-when xyz)? How does
;;       it behave on reloading/compiling the file
(set-dispatch-macro-character #\# #\m
			      #'(lambda (str char n)
				  (declare (ignore char n))
				  (list 'sdl2:in-main-thread () (read str t nil t))))


;; quite clunky, but with the right abstraction this can be done with even less typing
;; #m (progn
;;      (gl:bind-vertex-array *vao*)
;;      (%gl:buffer-sub-data :array-buffer (* 108 4) (* 72 4) *tex-coord*)) ;<- provide texture


;;; HOW TO USE:
;;;
;;; First, run this. It is SAFE to run repeatedly:
;;;
   (sdl2.kit:start)

;;;
;;; Then, make a window.
;;;
;;;   (make-instance 'sdl2.kit.test:texture-window)
;;;
;;; After you close a window, it will be collected at some point.

;;; You should NOT call any protocol functions on a window, except the
;;; following:
;;;
;;;   (render WINDOW)
;;;   (close-window WINDOW)
;;;
;;; These are the only functions guaranteed to be "safe" (including
;;; threadsafety and other expectations).

(defclass texture-window (kit.sdl2:gl-window)
  ((start-time :initform (get-internal-real-time))
   (one-frame-time :initform (get-internal-real-time))
   (frames :initform 0)))




;;Data:--------------------------------------------------------------------------

;; (defparameter *cube-positions*
;;   (cffi:foreign-alloc
;;    :float
;;    :initial-contents
;;    '(0.5 0.5 0.5			;0
;;      0.5 -0.5 0.5			;1
;;      -0.5 -0.5 0.5			;2
;;      -0.5 0.5 0.5			;3
;;      0.5 0.5 -0.5			;4
;;      0.5 -0.5 -0.5			;5
;;      -0.5 -0.5 -0.5			;6
;;      -0.5 0.5 -0.5)))		;7

;; front

(defvar *cube-positions*
  (cffi:foreign-alloc
   :float
   :initial-contents
   (list 0.5 0.5 0.5 0.5 -0.5 0.5 -0.5 -0.5 0.5 -0.5 -0.5 0.5 -0.5 0.5 0.5 0.5 0.5 0.5
	 0.5 0.5 -0.5 0.5 -0.5 -0.5 0.5 -0.5 0.5 0.5 -0.5 0.5 0.5 0.5 0.5 0.5 0.5 -0.5
	 -0.5 0.5 -0.5 -0.5 -0.5 -0.5 0.5 -0.5 -0.5 0.5 -0.5 -0.5 0.5 0.5 -0.5 -0.5 0.5
	 -0.5 -0.5 0.5 0.5 -0.5 -0.5 0.5 -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 0.5 -0.5
	 -0.5 0.5 0.5 0.5 0.5 -0.5 0.5 0.5 0.5 -0.5 0.5 0.5 -0.5 0.5 0.5 -0.5 0.5 -0.5
	 0.5 0.5 -0.5 0.5 -0.5 0.5 0.5 -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 -0.5 -0.5
	 -0.5 0.5 0.5 -0.5 0.5)))


(defvar *tex-coord*
  (cffi:foreign-alloc
   :float
   :initial-contents
   '(20.0 0.0   20.0 20.0   0.0 20.0   0.0 20.0   0.0 0.0   20.0 0.0 ;; to show of *sampler* :repeat
     1.0 0.0   1.0 1.0   0.0 1.0   0.0 1.0   0.0 0.0   1.0 0.0
     1.0 0.0   1.0 1.0   0.0 1.0   0.0 1.0   0.0 0.0   1.0 0.0
     1.0 0.0   1.0 1.0   0.0 1.0   0.0 1.0   0.0 0.0   1.0 0.0
     1.0 0.0   1.0 1.0   0.0 1.0   0.0 1.0   0.0 0.0   1.0 0.0
     1.0 0.0   1.0 1.0   0.0 1.0   0.0 1.0   0.0 0.0   1.0 0.0)))


(defvar *cube-indices*
  (cffi:foreign-alloc
   :unsigned-short
   :initial-contents
   '(;; front
     0 1 2
     2 3 0
     ;; right-side
     4 5 1
     1 0 4
     ;; back
     7 6 5
     5 4 7
     ;; left-side
     3 2 6
     6 7 3
     ;; top
     4 0 3
     3 7 4
     ;; bottom
     1 5 6
     6 2 1)))



;;Shader------------------------------------------------------------------------

;; the returned dictionary with the programs can be used like so:
;; (1) get the program directly (find-program <compiled-dictionary> <program-name>)
;; (2) or just use it directly (use-program <compiled-dictionary> <program-name>)
;;     also (use-program 0) works
(defun load-shaders ()
  (defdict shaders (:shader-path
		    (merge-pathnames
		     #p "shaders/" (asdf/system:system-source-directory :picking-sticks)))
    ;; instead of (:file <path>) you may directly provide the shader as a string containing the
    ;; source code
    (shader matrix-perspective-v :vertex-shader (:file "transform-and-project.vert"))
    (shader texture-f :fragment-shader (:file "2d-texture.frag"))
    ;; here we compose the shaders into programs, in this case just one ":basic-projection"
    (program :basic-projection (:model-to-clip :perspective-matrix :test-texture) ;<- UNIFORMS!
	     (:vertex-shader matrix-perspective-v)
	     (:fragment-shader texture-f)))
  ;; function may only run when a gl-context exists, as its documentation
  ;; mentions
  (compile-shader-dictionary 'shaders))

(defvar *programs-dict*)

(defun initialize-program ()
  (setf *programs-dict* (load-shaders)))

;; to be understood while reading the LOAD-SHADER function
;; example: (uniform :vec :<name-of-uniform> <new-value>)
(defgeneric uniform (type key value)
  (:method ((type (eql :vec)) key value)
    (uniformfv *programs-dict* key value))

  (:method ((type (eql :vec)) key value)
    (uniformfv *programs-dict* key value))

  (:method ((type (eql :mat)) key value)
    ;; nice, transpose is NIL by default!
    (uniform-matrix *programs-dict* key 4 value NIL))

  (:method ((type (eql :int)) key value)
    (uniformi *programs-dict* key value)))


;;VAO setup.....................................................................

(defvar *vao* 0)

(defun initialize-vao ()
  (let ((vao (first (gl:gen-vertex-arrays 1)))
	(vbo (first (gl:gen-buffers 1)))
	(ibo (first (gl:gen-buffers 1))))
    (gl:bind-vertex-array vao)
    ;;VBO
    (gl:bind-buffer :array-buffer vbo)
    ;;VBO - positions
    ;;to avoid magic numbers (* 24 4 3 2) the gl:gl-array (gl:alloc-gl-array) can be used,
    ;;which is a struct containing field with a pointer to the forein-memory and a field
    ;;with the size.  In this case ommited for the sake of a terse example.
    ;; Layout:
    ;; 8 number of vertices/colors
    ;; 3 components per vertex
    ;; 4 size of float
    ;; second (texture coordinate)
    ;; 8 vertices need to be associated with 2d-texture corner
    ;; 2 2d-texture needs two indices to read from it
    ;; 4 size of float
    (%gl:buffer-data :array-buffer (+ (* 108 4) (* 72 4)) *cube-positions* :static-draw)
    (%gl:enable-vertex-attrib-array 0)
    (%gl:vertex-attrib-pointer 0 3 :float :false 0 0)
    ;;VBO - texture coordinates
    ;; texture sub-data starts in vbo exactly after the position vertices hence
    (%gl:buffer-sub-data :array-buffer (* 108 4) (* 72 4) *tex-coord*)
    (%gl:enable-vertex-attrib-array 5)
    (%gl:vertex-attrib-pointer 5 2 :float :false 0 (* 108 4))

    ;;IBO
    (gl:bind-buffer :element-array-buffer ibo)
    ;; why (* 36 2)?
    ;; it takes 2 triangles to draw the side of cube, hence to draw a whole cube:
    ;; (* 2 6) => 12. Each triangle consists of 3 vertices, hence, (* 3 12) => 36
    ;; and the index buffer's indices first point to the vertices in the vbo,
    ;; supplied by *cube-positions*
    (%gl:buffer-data :element-array-buffer (* 2 36) *cube-indices* :static-draw)

    (gl:bind-vertex-array 0)
    (setf *vao* vao)))

;;utils-------------------------------------------------------------------------

(defun framelimit (window &optional (fps 60))
  "Issues SDL2:DELAY's to get desired FPS."
  (with-slots (one-frame-time) window
    (let ((elapsed-time (- (get-internal-real-time) one-frame-time))
	  (time-per-frame (/ 1000.0 fps)))
      (when (< elapsed-time time-per-frame)
	(sdl2:delay (floor (- time-per-frame elapsed-time))))
      (setf one-frame-time (get-internal-real-time)))))


(defun display-fps (window)
  (with-slots (start-time frames) window
    (incf frames)
    (let* ((current-time (get-internal-real-time))
	   (seconds (/ (- current-time start-time) internal-time-units-per-second)))
      (when (> seconds 5)
	(format t "FPS: ~A~%" (float (/ frames seconds)))
	(setf frames 0)
	(setf start-time (get-internal-real-time))))))


;;init code---------------------------------------------------------------------

(defvar *tex-unit* 0)
(defvar *tex-unit-1* 1)
(defvar *tex-unit-2* 2)

(defmethod initialize-instance :after ((w texture-window) &key &allow-other-keys)
  ;; GL setup can go here; your GL context is automatically active,
  ;; and this is done in the main thread.

  ;; if you (setf (idle-render window) t) it'll call RENDER as fast as
  ;; possible when not processing other events - suitable for games
  (setf (idle-render w) t)
  (gl:clear-color 0 0 0.5 1)
  (gl:clear :color-buffer-bit)
  (gl:viewport 0 0 800 600)

  ;; with culling
  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)

  (initialize-program)
  (initialize-vao)

  ;;texture
  ;; here we associate the uniform sample with the texture image unit
  (use-program *programs-dict* :basic-projection)
  (uniform :int :test-texture *tex-unit*) ; = glUniform1i(<location>, <texture-image-unit>);
  (use-program *programs-dict* 0)

  (create-texture)

  ;; TODO: clean this up and do the tests
  ;; enable v-sync 0, disable with 0 TODO: test with moving triangle at high velocity
  ;; if tearing disappears, or if it is an OS issue
  ; (sdl2:gl-set-swap-interval 1)
  )


;;; Getting image data from pictures with sdl2-image or opticl

;;sdl2-image experiments--------------------------------------------------------

;; (sdl2-image:init '(:png :jpg :tif))


;; TODO add more useful sdl-surface struct fields. type information?
(defclass image-object ()
  ((height :initarg :height)
   (width :initarg :width)
   (pixels :initarg :pixels)
   (ffi-array)))

(defmethod initialize-instance :after ((the-image image-object) &key)
  (alexandria:if-let ((pixels (slot-value the-image 'pixels)))
    (setf (slot-value the-image 'ffi-array)
  	  ;; TODO: get type from #3Array?
  	  (cffi:foreign-alloc :unsigned-char :initial-contents pixels))
    ;; else
    (error "The PIXELS slot is not set in the IMAGE-OBJECT object.")))

;; (defun image-file->sdl-surface->image-object (path)
;;   ;; TODO: when and why use. Initialize and load some formats (?)
;;   (sdl2-image:init '(:png :jpg :tif))
;;   (let* ((sdl-surface (sdl2-image:load-image path))
;; 	 (width (sdl2:surface-width sdl-surface))
;; 	 (height (sdl2:surface-height sdl-surface))
;; 	 ;; this is how we access fields from the ffi struct that are missing in
;; 	 ;; the wrapper!
;; 	 ;; NEXT-TODO: convert into rgba, use "pitch" to read row of pixels properly
;; 	 ;; and read a bit into sdl_surface to see if you missing something. As things
;; 	 ;; are right now opticl appears much simpler. It also doesn't need ffi.
;; 	 (pitch (plus-c:c-ref sdl-surface sdl2-ffi:sdl-surface :pitch))
;;; NEXT-TODO: very useful
;; sdl-surface->format->rmask directly translates to:
;; (c-ref some-surface sdl2-ffi:sdl-surface :format :rmask)
;; and then there is C-LET:

;; even more powerful abstraction example:
;; (plus-c:c-let ((surface sdl2-ffi:sdl-surface :from *sdl-surface*))
;; 	(surface :format :rmask))


;; > it will also allocate them if you don't specify :from/:ptr, and you can
;;        iirc specify :count


;; > basically should fold all the syntax for alloc/ref/free/etc into one
;;        handy easy-to-read block

;; > : do the pointers get freed outside the c-let? 

;; > : c-let doesn't free, C-WITH does free, or either explicitly with
;;        :free {t|nil}

;; 	 (pixels-ptr (plus-c:c-ref sdl-surface sdl2-ffi:sdl-surface :pixels))
;; 	 )
;;     (declare (ignore pitch)) ;; TODO
;;     ;; TODO: :pixel slot must be an object INITIALIZE-INSTANCE :after will build a ffi-array
;;     ;; from
;;     (make-instance 'image-object :width width :height height :pixels NIL)))




;; (defvar *123-image-object-from-sdl-surface* (image-file->sdl-surface->image-object "123.png"))

;; the sdl-surface is a struct-object
;; (sdl2-ffi::sdl-surface-ptr *123-PNG-SDL-SURFACE*) ==> ptr
;; ^ iterating over the above, reading bytes, shows that the picture colors
;; are indeed in there, but with some other image data.

;; new approach: get missing slot in the sdl2 wrapper using plus-c:c-ref
;; (plus-c:c-ref (sdl2-image:load-image "123.png)
;; *123-png-sdl-surface* sdl2-ffi:sdl-surface :pixels)


;;opticl experiments------------------------------------------------------------

;; TODO: chedk out 3bgl-opticl for a direct solution
;; TODO: check out sbcl for some lowlevel vecor access and provide it with
;;       #+sbcl.. ?
(defun 3d-array->vector (3d-array)
  (let* ((dims (array-dimensions 3d-array))
	 (dims-1d (apply #'* dims))
	 (vector (make-array dims-1d)))
    (loop for i below dims-1d do
	 (setf (aref vector i) (row-major-aref 3d-array i)))
    vector))


(defun image-file->image-object (path)
  (let ((img (convert-image-to-rgba (read-png-file path))))
    ;; first dim: height then width then length of pixel, here 4 because rgba
    (with-image-bounds (height width) img
      (make-instance 'image-object :height height :width width
		     :pixels (3d-array->vector img)))))

;;print representation reads, intuitively, top-to-bottom just like the picture rendered
;;a 3d-array: 1.row 2.pixel 3. color-component
(defvar *png-opticl* (image-file->image-object "foo.png"))

;;useful functions
;; (with-image-bounds (height width) *png-opticl*
;;   (list height width)) ; ==> (3 3)
;; (convert-image-to-rgba *PNG-OPTICL*)

;; since we're dealing with a simple array
;; (array-element-type <array>)
;; (array-dimensions <array>)


;;texture data------------------------------------------------------------------

(defvar *some-texture-data* (cffi:foreign-alloc :unsigned-char :initial-contents
						(list 10 0 0 0 0 (floor 255 2) 0 0 0 255)))


(defvar *2d-texture-data*
  (cffi:foreign-alloc :unsigned-char :initial-contents (loop for i upto 255 collect i)))


(defvar *arrow-lambda-data*
  (cffi:foreign-alloc :unsigned-char
		      ;; to tell up from bottom better in tests
		      ;; Note: This is also how opengl expects texture data. We can read the value from this
		      ;; 1d-array by making it arbitrarily 2d (gl:tex-image-2d ... <width> <height> .#|<-here|#.)
		      ;; and we read the value using tex-coordinate, where, with the current representation below
		      ;; [0,0] represents top-left, [1,0] top-righ, [1,1]. If we imagine the texture coordinates
		      ;; following a coordinate system sceme this "asci-art" below is upside-down. Positive
  		      ;; y is down, negative up. But x is right.
  		      :initial-contents
 ;;    	 -------+------------------------------------------------------------------------------> +x
 #|	       	.  |# (list  64  64  64  255 64  64  64  64 000  64  64  64  64  64 000  64 
 #|	       	.  |# 	     64  64  255 255 255 64  64 000  64  64 100  64  64  64  64 000 
 #|	       	.  |# 	     64  255 64  255 64  255 64 000  64  64  64 100  64  64  64 000 
 #|	       	.  |# 	     64  64  64  255 64  64  64 000  64  64  64 100  64  64  64 000 
 #|	       	.  |# 	     64  64  64  255 64  64  64 000  64  64  64 100  64  64  64 000 
 #|	       	.  |# 	     64  64  64  255 64  64  64 000  64  64  64 100 100  64  64 000 
 #|	       	.  |# 	     64  64  64  255 64  64  64 000  64  64 100  64 100  64  64 000 
 #|	       	.  |# 	     64  64  64  64  64  64  64  64 000 100  64  64  64 100 000  64 
 #|	       	.  |# 	     128 128 128 128 128 128 128 128 195 195 195 195 195 195 195 195 
 #|	       	.  |# 	     128 128 128 128 128 128 128 128 195 195 195 195 195 195 195 195 
 #|	       	.  |# 	     128 128 128 128 128 128 128 128 195 195 195 195 195 195 195 195 
 #|	       	.  |# 	     128 128 128 128 128 128 128 128 195 195 000 195 195 195 195 195 
 #|	       	.  |# 	     000 128 128 128 128 128 128 000 195 000 000 000 195 000 000 000 
 #|	       	.  |# 	     000 128 000 128 128 128 000 128 195 195 000 195 195 195 195 000 
 #|     +y 0	.  |# 	     000 000 128 128 000 128 128 000 195 195 000 195 195 195 000 195 
 #|     	v 0|# 	     000 128 000 128 128 128 000 128 195 195 000 195 195 000 000 000)))


(defmacro rgba-list (&body list)
  `(let
       ((r '(255 0 0 255))
	(g '(0 255 0 255))
	(b '(0 0 255 255))
	(d '(0 0 0 255)) ;; dark
	(v '(138 43 226 255))
	(_ '(255 255 255 0)))
     (append
      ,@list)))


(defvar *rgba-texture-data*
  (cffi:foreign-alloc :unsigned-char
  		      :initial-contents
		      ;; TODO: test alpha zero "discard" and stamp out a sprite?
		      (rgba-list _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
				 _ b r _ _ b r _ g b d _ d r g _ _ _ _ _ 
				 _ _ g _ _ _ g _ _ r _ _ b _ d _ _ _ _ _ 
				 _ _ b _ _ _ b _ _ _ d _ g r _ _ _ _ _ _ 
				 _ _ d g r _ d _ r g b _ r _ _ _ _ _ _ _ 
				 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
				 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
				 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
				 _ g g g _ _ r _ _ b _ b _ d d _ v v v _
				 _ g _ _ _ r _ r _ b b b _ d _ _ v _ _ _
				 _ g _ _ _ r r r _ b _ b _ d d _ _ v _ _
				 _ g _ g _ r _ r _ b _ b _ d _ _ _ _ v _
				 _ _ g _ _ r _ r _ b _ b _ d d _ v v v _
				 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
				 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
				 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
				 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
				 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
				 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
				 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ )))

;;(defparameter *123-texture-data*)

(defvar *sampler* 0)
(defvar *texture* 0)


(defun create-texture ()
  (let ((m-texture (first (gl:gen-textures 1)))
	;;(width 256) ; length of the look-up table
	(texture-data *arrow-lambda-data*))

    (setf *texture* m-texture)

    ;; :texture-2d the texture contains 2d-image_s_. Peculiariy: once you bind the
    ;; texture with a certain type, here :texture-2d, you always need to bind it
    ;; with the same type
    (%gl:bind-texture :texture-2D m-texture)

    ;; Texture contain arrays (called images), the elements of the images are called texels

    ;; Though textures have a lot in common with buffer objects there some major difference:
    ;; when we provide data to a buffer object owned by gl with gl:buffer-sub-data opengl will
    ;; get the data in the same way we provided it, you could say we memcpy the exact same
    ;; bytes over to the GPU memory.
    ;; With textures, though, this is wildly different. The gl will take the data from the
    ;; data from us and put it in whatever representation is most efficient for the underlying
    ;; hardware. Our job thus is to do two things:
    ;; (1) tell OpenGL what format to use (underlying byte representation of data will vary
    ;;     depending on the graphics card)
    ;; (2) and describe how the data is stored in our array
    ;;

    ;; This process of transfering the user provided data representation to a texture image
    ;; is called _/pixel transfer/_!

    ;; Both is accomplied using gl:tex-image-*, which describes how we allocate storage
    ;; and pass data to the texture, like gl:buffer-data
    ;; the parameters:
    ;; - target         : type of the _currently_ bound texture
    ;; - level          : TODO, UPDATE: used for mipmaps when the same texture is provided
    ;;                    at various resolutions to solve the issue of a very small texture
    ;;                    which would have multiple texels per pixel, and OpenGL (ES atleast)
    ;;                    can only really take 4 into comparison calculations (magnimization).
    ;; - internal-format: format that opengl will use to store the texture's data
    ;; - width          : width of the image = length of the look-up/array table
    ;; - border         : must always be 0: represents an old feature, no longer supported

    ;; These last three parameters, of all the functions of the form gl:tex-image*, are special.
    ;; They tell opengl how to read the texture data in our array..
    ;; - format         : component supplied to the "uniform sampler", ther are only certain
    ;;                    arrangments: :red works, :blue doesn't, but :rgba does
    ;; - type           : type representation on the opengl side (as opposed to user data), OpenGL
    ;;                    will perform type conversions in the pixel transfer step
    ;; - data           : actuall data provided
    (gl:tex-image-2d :texture-2d
		     0 ; level
		     ;; the suffix of the format represent the data type:
		     ;; here: f = float
		     ;; no suffix defaults to the most commonly used: unsigned normalized integers
		     :r8
		     16 ;; width = 1 means one component (not width 0!)
		     16 ;; height, we provide the data with a 1d-array, this is where opengl
		     ;; descides how to access it via pointer arithmetic to get at the proper values
		     ;; i.e. how many rows in a column is decided here

		     0 ; border: old no longer supported feature

		     ;; we are uploading a single "red" component to the texture,
		     ;; components of _texels_ are called after colors. Remember that our
		     ;; texel is going to be represented as a 4d-vector, and a vectors
		     ;; _components_ are it's x,y,z,w values, but for a texel those are
		     ;; the r,g,b,a values!!
		     ;;Because it doesn't end with "-integer" opengl knows that it is
		     ;;either a floating-point value or a normalized integer.  note that
		     ;;normalized integers are converted to float by opengl when they're
		     ;;accessed
		     :red
		     ;;normalized integer!!
		     ;; each component is stored in a float
		     :unsigned-byte
		     texture-data)
    ;; TODO understand
    (gl:tex-parameter :texture-2d :texture-base-level 0)
    (gl:tex-parameter :texture-2d :texture-max-level 0)
    ;; unbind
    (gl:bind-texture :texture-2D 0)

    ;; the _sampler object_ specifies how data should be read from the texture
    (setf *sampler* (first (gl:gen-samplers 1)))
    ;; TODO explain
    (gl:sampler-parameter *sampler* :texture-mag-filter :nearest)
    (gl:sampler-parameter *sampler* :texture-min-filter :nearest)


    ;; :texture-wrap-s tells opengl that texture coordinates should be clampled to the
    ;; range of the texture. Big peculiarity the "-s" at the end actually refers to the
    ;; first component of the texture, (stpq)
    (gl:sampler-parameter *sampler* :texture-wrap-s :repeat) ;; change to repeat
    ;; now with 2d-textures we have two texture coordinates we need to cara about:
    (gl:sampler-parameter *sampler* :texture-wrap-t :repeat)


    ;; Now in our shader we have a "uniform sampler1D <name>" and we need to associate
    ;; this sampler uniform with our texture object (here: m-texture or *texture*) like
    ;; with UBO we do this with a slot in the context, the so called:
    ;; _/texture image unit/_
    ;; we associated


    (setf *texture* m-texture)))


;;Rendering----------------------------------------------------------------------

(defparameter *rotate-x* 1.0)
(defparameter *rotate-y* 0.0)
(defparameter *zoom-z* -2.0)

(defvar *use-texture-number* 1) ;; for live-coding tests

(defun update-texture ()
  ;; expects a gl-program already in use
  ;; TODO: ugh, so much repetition
  (case *use-texture-number*
    (0 (progn
	 ;; this tells the shader sampler uniform where in the texture image unit
	 ;; to fetch data from
	 (%gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) *tex-unit-1*))
	 (%gl:bind-texture :texture-2d *texture*)
	 (%gl:bind-sampler *tex-unit-1* *sampler*)
	 (gl:sampler-parameter *sampler* :texture-mag-filter :nearest)
	 (gl:sampler-parameter *sampler* :texture-min-filter :linear)


	 (uniform :int :test-texture *tex-unit-1*)

	 (gl:tex-image-2d :texture-2d 0 :rgba8 20 20 0
			  :rgba	;components per element
			  :unsigned-byte ;; normalized integer
			  *rgba-texture-data*)))
    (1 (progn
	 (%gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) *tex-unit*))
	 (%gl:bind-texture :texture-2d *texture*)
	 (%gl:bind-sampler *tex-unit* *sampler*)
	 (uniform :int :test-texture *tex-unit*)
	 (gl:tex-image-2d :texture-2d
			  0		;level TODO
			  :r8		;individual element representation
			  16		;width
			  16		;height
			  0		; border
			  :red	;components per element
			  :unsigned-byte ;; normalized integer
			  *arrow-lambda-data*)))
    (2 (progn
	 ;; this tells the shader sampler uniform where in the texture image unit
	 ;; to fetch data from
	 (%gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) *tex-unit-2*))
	 (%gl:bind-texture :texture-2d *texture*)
	 (%gl:bind-sampler *tex-unit-2* *sampler*)
	 (gl:sampler-parameter *sampler* :texture-mag-filter :nearest)
	 (gl:sampler-parameter *sampler* :texture-min-filter :linear)


	 (uniform :int :test-texture *tex-unit-2*)

	 (with-slots (width height pixels ffi-array) *png-opticl*
	   (gl:tex-image-2d :texture-2d 0 :rgba8
			    width
			    height 0
			    :rgba	;components per element
			    :unsigned-byte ;; normalized integer
			    ffi-array))))))

(defun draw-cube ()
  (gl:bind-vertex-array *vao*)
  (use-program *programs-dict* :basic-projection)
  ;; all the neat transformations take place here
  (uniform :mat :model-to-clip
	   (vector
	    (sb-cga:matrix*
	     (sb-cga:translate (vec3 0.0 0.0 *zoom-z*))
	     (sb-cga:rotate (vec3 *rotate-x* *rotate-y* 0.0))
	     ;;(sb-cga:rotate (vec3 0.0 (mod (/ (sdl2:get-ticks) 5000.0) (* 2 3.14159)) 0.0))
	     )))
  ;; projection matrix
  (uniform :mat :perspective-matrix
	   (vector (perspective-matrix (* pi 1/3) 1/1 0.0 1000.0)))

  ;;texture stuff:
  ;; set the texture image unit every subsequent gl:bind-texture will associate texture with
  ;;(%gl:active-texture (+ (cffi:foreign-enum-value '%gl:enum :texture0) *tex-unit*))
  ;; associate our *texture*(-handle) with the texture image unit above. Remember the GLSL sampler uniform
  ;; association is build with setting the uniform to the same texture image unit we used in the
  ;; gl:active-texture (the *tex-unit* value) this was already done in the INITILIZE-INSTANCE code
  ;; with (uniform :int :test-texture *tex-unit*)
  ;; Another very important part of gl:active-texture is that it the texture now is "bound to the context"
  ;; and as such many gl calls can affect it, like: gl:tex-image, gl:tex-parameter, gl:bind-texture:

  

  (update-texture)

  ;; (%gl:draw-elements :triangles (* 36 2) :unsigned-short 0)
  (%gl:draw-arrays :triangles 0 (* 2 36))


  (%gl:bind-sampler *tex-unit* 0)
  (%gl:bind-texture :texture-2d 0)
  (gl:bind-vertex-array 0))


(defmethod render ((window texture-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW  (!!)
  ;; after RENDER.
  (gl:clear :color-buffer)

  (draw-cube)

  (display-fps window)
  (framelimit window 60))

;;Events------------------------------------------------------------------------

(defmethod close-window ((window texture-window))
  (format t "Bye!~%")
  ;; To _actually_ destroy the GL context and close the window,
  ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
  ;; prompt the user!
  (call-next-method))

(defmethod mousewheel-event ((window texture-window) ts x y)
  ;; zoom in/out
  (cond ((= y 1) (incf *zoom-z* 0.2))
	((= y -1) (decf *zoom-z* 0.2)))
  (render window))


(defmethod keyboard-event ((window texture-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (when (eq :scancode-escape scancode)
      (close-window window))))

(defmethod mousebutton-event ((window texture-window) state ts b x y)
  (format t "~A button: ~A at ~A, ~A~%" state b x y))

(defmethod mousemotion-event ((window texture-window) ts mask x y xr yr)
  (flet ((left-mouse-button-clicked-p ()
	   (= mask 1)))
    ;; rotate x, y axis
    (when (left-mouse-button-clicked-p)
      (incf *rotate-y* (/ xr 100.0))
      (incf *rotate-x* (/ yr 100.0)))))
