(defpackage :test
  ;; :use inherits all the exported symbols from the package given
  (:use :cl
	;; this one's important, as all the defclass lambda lists
	;; and the the implicitly created generic functions
	;; appear in the example unqualified, as they're interned
	:sdl2.kit))

(in-package :test)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; HOW TO USE:
;;;
;;; First, run this.  It is SAFE to run repeatedly:
;;;
;;;   (sdl2.kit:start)
;;;
;;; Then, make a window.
;;;
;;;   (make-instance 'sdl2.kit.test:test-window)
;;;
;;; You can make multiple windows if you want.  Note that, despite not
;;; assigning the value, THIS IS NOT COLLECTED.  A reference is kept
;;; to all windows:
;;;
;;;   (all-windows)
;;;
;;; After you close a window, it will be collected at some point.
;;;
;;; You should NOT call any protocol functions on a window, except the
;;; following:
;;;
;;;   (render WINDOW)
;;;   (close-window WINDOW)
;;;
;;; These are the only functions guaranteed to be "safe" (including
;;; threadsafety and other expectations).

(defclass test-window (kit.sdl2:gl-window)
  ((rotation :initform 0.0)
   (start-time :initform (get-internal-real-time))
   (frames :initform 0)))

;;; All of these methods are OPTIONAL.  However, without a render
;;; method, your window will not look like much!


;;; Note this is an :AFTER method.  You should either use :AFTER, or
;;; you must (CALL-NEXT-METHOD).

(defmethod initialize-instance :after ((w test-window) &key &allow-other-keys)
  ;; GL setup can go here; your GL context is automatically active,
  ;; and this is done in the main thread.
  (setf (idle-render w) t)
  (gl:viewport 0 0 800 600)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod render ((window test-window))
  ;; Your GL context is automatically active.  FLUSH and
  ;; SDL2:GL-SWAP-WINDOW are done implicitly by GL-WINDOW
  ;; after RENDER.
  (with-slots (rotation) window
    (gl:load-identity)
    (gl:rotate rotation 0 0 1)
    (gl:clear-color 0.0 0.0 1.0 1.0)
    (gl:clear :color-buffer)
    (gl:begin :triangles)
    (gl:color 1.0 0.0 0.0)
    (gl:vertex 0.0 1.0)
    (gl:vertex -1.0 -1.0)
    (gl:vertex 1.0 -1.0)
    (gl:end))
  (with-slots (start-time frames) window
    (incf frames)
    (let* ((current-time (get-internal-real-time))
           (seconds (/ (- current-time start-time) internal-time-units-per-second)))
      (when (> seconds 5)
        (format t "FPS: ~A~%" (float (/ frames seconds)))
        (setf frames 0)
        (setf start-time (get-internal-real-time))))))

(defmethod close-window ((window test-window))
  (format t "Bye!~%")
  ;; To _actually_ destroy the GL context and close the window,
  ;; CALL-NEXT-METHOD.  You _may_ not want to do this, if you wish to
  ;; prompt the user!
  (call-next-method))

(defmethod mousewheel-event ((window test-window) ts x y)
  (with-slots (rotation) window
    (incf rotation (* 12 y))
    (render window)))

(defmethod textinput-event ((window test-window) ts text)
  (format t "You typed: ~S~%" text)
  (when (string= "Q" (string-upcase text))
    (close-window window)))

(defmethod keyboard-event ((window test-window) state ts repeat-p keysym)
  (let ((scancode (sdl2:scancode keysym)))
    (unless repeat-p
      (format t "~A ~S~%" state scancode))
    (when (eq :scancode-escape scancode)
      (close-window window))))

(defmethod mousebutton-event ((window test-window) state ts b x y)
  (format t "~A button: ~A at ~A, ~A~%" state b x y))

(defmethod mousemotion-event ((window test-window) ts mask x y xr yr)
  (when (> mask 0)
    (format t "Mouse motion, button-mask = ~A at ~A, ~A~%" mask x y)))

;; (make-instance 'test-window)
