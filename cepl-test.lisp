;; to get this to work:
;; load CEPL's "cepl-default" system
;; then (cepl:repl)
;; then (run-loop)


;; (defpackage cepl-test
;;   (:use :cl :cepl))

;; TODO: for some strange reason it doesn't work with :cepl-test repl which :use :cepl 
;; (in-package :cepl-test)

(in-package :cepl)

(defparameter *array* nil)
(defparameter *stream* nil)
(defparameter *running* nil)

(defstruct-g pos-col ()
  (position :vec3 :accessor pos)
  (color :vec4 :accessor col)
  (tex :vec2))

(defun-g vert ((vert pos-col))
  (values (v! (pos vert) 1.0)
          (col vert)))

(defun-g frag ((color :vec4))
  color)

(defpipeline prog-1 ()
    (g-> #'vert #'frag))

(defun step-demo ()
  (evt:pump-events)
  (update-swank)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (map-g #'prog-1 *stream*)
  (update-display))

;; start it using this
(defun run-loop ()
  (setf *running* t
        *array* (make-gpu-array
                 (list (list (v!  0.5 -0.36 0) (v! 0 1 0 1) (v! -1 1))
                       (list (v!    0   0.5 0) (v! 1 0 0 1) (v! 1 1))
                       (list (v! -0.5 -0.36 0) (v! 0 0 1 1) (v! 0 -1)))
                 :element-type 'pos-col)
   *stream* (make-buffer-stream *array*))
  (loop :while *running* :do (continuable (step-demo))))

;; to terminate the loop, test with other WM, with stumpwm the last image
;; stays imprinted in the sdl-window
(defun stop-loop ()
  (setf *running* nil))

(evt:observe (evt:|sys|)
  (when (typep e 'evt:will-quit) (stop-loop)))
