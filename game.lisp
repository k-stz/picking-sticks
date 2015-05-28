;; sbcl 2.6% mem

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :test
  ;; :use inherits all the exported symbols from the package given
  (:use :cl
	;; this one's important, as all the defclass lambda lists
	;; and the the implicitly created generic functions
	;; appear in the example unqualified, as they're interned
	:sdl2.kit
	:kit.gl.shader
	:kit.math))

(in-package :test)



;;; HOW TO USE:
;;;
;;; First, run this. It is SAFE to run repeatedly:
;;;
   (sdl2.kit:start)

;;;
;;; Then, make a window.
;;;
;;;   (make-instance 'sdl2.kit.test:test-window)
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

(defclass test-window (kit.sdl2:gl-window)
  ((start-time :initform (get-internal-real-time))
   (one-frame-time :initform (get-internal-real-time))
   (frames :initform 0)
   (rotation :initform 0.0)))

;;; All of these methods are OPTIONAL.  However, without a render
;;; method, your window will not look like much!


;;; Note this is an :AFTER method.  You should either use :AFTER, or
;;; you must (CALL-NEXT-METHOD).


(defvar *triangle-data*
  (cffi:foreign-alloc :float
		      :initial-contents '(-0.5 -0.5 0.0 1.0
					  0.0 0.5 0.0 1.0
					  0.5 -0.5 0.0 1.0)))

(defvar *cube-data*
  (cffi:foreign-alloc
   :float
   :initial-contents
   '(0.5 0.5 0.5			;0
     0.5 -0.5 0.5			;1
     -0.5 -0.5 0.5			;2
     -0.5 0.5 0.5			;3
     0.5 0.5 -0.5			;4
     0.5 -0.5 -0.5			;5
     -0.5 -0.5 -0.5			;6
     -0.5 0.5 -0.5)))			;7

(defvar *cube-indices*
  (cffi:foreign-alloc
   :unsigned-short
   :initial-contents
   '(0 1 2
     2 3 0

     4 5 1  
     1 0 4

     7 6 5
     5 4 7

     3 2 6
     6 7 3

     4 0 3
     3 7 4

     1 5 6
     6 2 1)))


;;Shader------------------------------------------------------------------------


;; the returns dictioary with the program can be used like so:
;; (1) get the program directly (find-program <compiled-dictionary> <program-name>
;; (2) or just use it directily (use-program <compiled-dictionary> <program-name>
;;     also (use-program 0) works
(defun load-shaders ()
  (defdict shaders (:shader-path
		    ;; TODO: check out tradewarz' GET-PATH
		    (merge-pathnames
		     #p "shaders/" (asdf/system:system-source-directory :picking-sticks)))
    (shader pass-through-v :vertex-shader (:file "pass-through.vert"))
    (shader one-transform-v :vertex-shader (:file "one-transform.vert"))
    (shader matrix-perspective-v :vertex-shader (:file "transform-and-project.vert"))
    (shader same-color-f :fragment-shader (:file "same-color.frag"))
    (program :basic (:color) ;; <- UNIFORMS!
	     (:vertex-shader pass-through-v)
	     (:fragment-shader same-color-f))
    (program :basic-transform (:model-to-clip :color)
	     (:vertex-shader one-transform-v)
	     (:fragment-shader same-color-f))
    (program :basic-projection (:model-to-clip :perspective-matrix :color)
	     (:vertex-shader matrix-perspective-v)
	     (:fragment-shader same-color-f)))
  ;; funciton may only run when a gl-context exists, as it's documentation
  ;; mentions
  (compile-shader-dictionary 'shaders))

(defvar *programs-dict*)

(defun initialize-program ()
  (setf *programs-dict* (load-shaders)))


;; to be understood while reading LOAD-SHADER function
;; example: (uniform :vec :<name-of-uniform> <new-value>)
(defmethod uniform ((type (eql :vec)) key value)
  (uniformfv *programs-dict* key value))


(defmethod uniform ((type (eql :mat)) key value)
  ;; nice, transpose is NIL by default!
  (uniform-matrix *programs-dict* key 4 value NIL))


;; TODO: why specialize on SHADE


;;..............................................................................

(defvar *vao* 0)
(defun initialize-vao ()
  (let ((vao (first (gl:gen-vertex-arrays 1)))
	(vbo (first (gl:gen-buffers 1)))
	(ibo (first (gl:gen-buffers 1))))
    (gl:bind-vertex-array vao)
    ;;VBO
    (gl:bind-buffer :array-buffer vbo)
    (%gl:buffer-data :array-buffer (* 24 4 3) *cube-data* :static-draw)
    (%gl:enable-vertex-attrib-array 0)
    ;;                           v-TODO this means 3 floats make up a vertex, right?
    (%gl:vertex-attrib-pointer 0 3 :float :false 0 0)
    ;;IBO
    (gl:bind-buffer :element-array-buffer ibo)
    (%gl:buffer-data :element-array-buffer (* 36 2) *cube-indices* :static-draw)

    ;;TODO: do you need to fill the arrays into gl first and _then_ bind them
    ;;after the vao implicitly?
    (gl:bind-vertex-array 0)
    (setf *vao* vao)))


(defvar *position-buffer-object*)



(defmethod initialize-instance :after ((w test-window) &key &allow-other-keys)
  ;; GL setup can go here; your GL context is automatically active,
  ;; and this is done in the main thread.

  ;; if you (setf (idle-render window) t) it'll call RENDER as fast as
  ;; possible when not processing other events; suitable for games
  (setf (idle-render w) t)
  (gl:clear-color 0 0 1 1)
  (gl:clear :color-buffer-bit)
  (gl:viewport 0 0 800 600)

  (gl:enable :cull-face)
  (gl:cull-face :back)
  (gl:front-face :cw)

  ;; shader stuff
  (initialize-program)

  ;; vao
  (initialize-vao)
  
  ;; enable v-sync 0, disable with 0 TODO: test with moving triangle at high velocity
  ;; if tearing disappears, or if it is an os issue
  ;; (sdl2:gl-set-swap-interval 1) 

  ;; setup buffer-data
  (setf *position-buffer-object* (first (gl:gen-buffers 1)))
  (gl:bind-buffer :array-buffer *position-buffer-object*)
  (%gl:buffer-data :array-buffer 48 *triangle-data* :static-draw)
  (initialize-program)
  (gl:enable-vertex-attrib-array 0))


(defun framelimit (window &optional (fps 60))
  "SDL2:DELAY to get desired FPS."
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



(defun draw-triangle ()
  (use-program *programs-dict* :basic-projection)
  (uniform :vec :color #(1.0 0.0 0.0 1.0))
  (uniform :mat :model-to-clip
	   (vector
	    (sb-cga:matrix*
	     (sb-cga:rotate (vec3 0.0 0.0 (mod (/ (sdl2:get-ticks) 5000.0) (* 2 3.14159))))
	     (sb-cga:translate (vec3 0.0 0.0 -1.0)))))

  (uniform :mat :perspective-matrix
	   (vector (sb-cga:identity-matrix)))  
  
  (gl:bind-buffer :array-buffer *position-buffer-object*)
  (%gl:enable-vertex-attrib-array 0)
  (%gl:vertex-attrib-pointer 0 4 :float :false 0 0)

  (gl:draw-arrays :triangles 0 3))


(defun draw-cube ()
  (gl:bind-vertex-array *vao*)
  (use-program *programs-dict* :basic-projection)
  (uniform :vec :color #(1.0 0.0 0.0 .1))
  (uniform :mat :model-to-clip
	   (vector
	    (sb-cga:matrix*
	     (sb-cga:translate (vec3 0.0 0.0 -2.0))
	     (sb-cga:rotate (vec3 1.0 0.0 0.0))
	     (sb-cga:rotate (vec3 0.0 (mod (/ (sdl2:get-ticks) 5000.0) (* 2 3.14159)) 0.0)))))

    (uniform :mat :perspective-matrix
	   (vector (perspective-matrix 90.0 3/4 1.0 1000.0)))  

  (%gl:draw-elements :triangles  (* 36 2) :unsigned-short 0))


(defmethod render ((window test-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW  (!!)
  ;; after RENDER.
  (gl:clear :color-buffer)

;  (draw-triangle)
  (draw-cube)

  (display-fps window)
  (framelimit window 60))

(defmethod close-window ((window test-window))
  (format t "Bye!~%")
  ;; To _actually_ destroy the GL context and close the window,
  ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
  ;; prompt the user!
  (call-next-method))

(defmethod mousewheel-event ((window test-window) ts x y)
  (with-slots (rotation) window
    (incf rotation (* 12 y))
    (render window))
  ;; (format t "Mousewheel-event issued ts:~a x:~x y:~y" ts x y)
  )

(defmethod textinput-event ((window test-window) ts text) ;
  ;; (format t "You typed: ~S~%" text)
  ;; (when (string= "Q" (string-upcase text))
  ;;   (close-window window))
  )

(defmethod keyboard-event ((window test-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (unless repeat-p
      (format t "~A ~S~%" state scancode))
    (when (eq :scancode-space scancode)
      (print "Space-key pushed"))
    (when (eq :scancode-escape scancode)
      (close-window window))))

(defmethod mousebutton-event ((window test-window) state ts b x y)
  (format t "~A button: ~A at ~A, ~A~%" state b x y))

(defmethod mousemotion-event ((window test-window) ts mask x y xr yr)
  (when (> mask 0)
    (format t "Mouse motion, button-mask = ~A at ~A, ~A~%" mask x y)))

;; (make-instance 'test-window)
