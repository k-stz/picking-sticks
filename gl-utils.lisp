(defpackage :gl-utils
  (:use :cl)
  (:export :with-vao
	   :with-program))

(in-package :gl-utils)

;; nesting these is usually a bad sign:
;; note how when we leave a with-vao the vao is always unbound
;; from the context, subsequent code depending on its state, such as a
;; gl:draw* that we're very likely to put in one of these, will fail
(defmacro with-vao (vao &body body)
  `(progn (gl:bind-vertex-array ,vao)
	  ,@body
	  (gl:bind-vertex-array 0)))

(defmacro with-program (program-dict program-name &body body)
  `(progn (kit.gl.shader:use-program ,program-dict ,program-name)
	  ,@body
	  (kit.gl.shader:use-program ,program-dict 0)))
