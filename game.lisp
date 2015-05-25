;; sbcl 2.6% mem

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :test
  ;; :use inherits all the exported symbols from the package given
  (:use :cl
	;; this one's important, as all the defclass lambda lists
	;; and the the implicitly created generic functions
	;; appear in the example unqualified, as they're interned
	:sdl2.kit
	:kit.gl.shader))

(in-package :test)



;;; HOW TO USE:
;;;
;;; First, run this.  It is SAFE to run repeatedly:
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
    (shader same-color-f :fragment-shader (:file "same-color.frag"))
    (program :basic (:color) ;; <- UNIFORMS!
	     (:vertex-shader pass-through-v)
	     (:fragment-shader same-color-f)))
  ;; funciton may only run when a gl-context exists, as it's documentation
  ;; mentions
  (compile-shader-dictionary 'shaders))

(defvar *programs-dict*)

(defun initialize-program ()
  (setf *programs-dict* (load-shaders)))

;; to be understood while reading LOAD-SHADER function
(defmethod uniform ((type (eql :vec)) key value)
  ;; TODO: abstract me (find-program .. >:basic<)
  (uniformfv *programs-dict* key value))




;;..............................................................................


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

  ;; shader stuff
  (initialize-program)

  
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
  (use-program *programs-dict* :basic)
  (uniform :vec :color #(1.0 0.0 0.0 1.0))
  (gl:bind-buffer :array-buffer *position-buffer-object*)
  (%gl:enable-vertex-attrib-array 0)
  (%gl:vertex-attrib-pointer 0 4 :float :false 0 0)

  (gl:draw-arrays :triangles 0 3))


(defmethod render ((window test-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW  (!!)
  ;; after RENDER.

  (draw-triangle)

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
