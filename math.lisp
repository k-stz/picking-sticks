(defpackage :math
  (:use :cl
	:kit.math)
  (:export :vec2
	   :vec2+
	   :vec2-
	   :vec2*
	   :vec3+
	   :vec3-
	   :vec3*
	   :mat4*vec3))

(in-package :math)

(defun vec2+ (v1 v2)
  (vec2 (+ (aref v1 0) (aref v2 0))
	(+ (aref v1 1) (aref v2 1))))


(defun vec2- (v1 &optional v2)
  (if (null v2)
      (vec2 (- (aref v1 0))
	    (- (aref v1 1)))
      (vec2 (- (aref v1 0) (aref v2 0))
	    (- (aref v1 1) (aref v2 1)))))


(defun vec2* (v1 scalar)
  (vec2 (* (aref v1 0) scalar)
	(* (aref v1 1) scalar)))


(defun vec3+ (v1 v2)
  (vec3 (+ (aref v1 0) (aref v2 0))
	(+ (aref v1 1) (aref v2 1))
	(+ (aref v1 2) (aref v2 2))))

(defun vec3- (v1 &optional v2)
  (if (null v2)
      (vec3 (- (aref v1 0))
	    (- (aref v1 1))
	    (- (aref v1 2)))
      (vec3 (- (aref v1 0) (aref v2 0))
	    (- (aref v1 1) (aref v2 1))
	    (- (aref v1 2) (aref v2 2)))))

(defun vec3* (v1 scalar)
  (vec3 (* (aref v1 0) scalar)
	(* (aref v1 1) scalar)
	(* (aref v1 2) scalar)))

(defun vec4->vec3 (vec4)
  "Discard the w component and return a vec3."
  (vec3 (aref vec4 0)
	(aref vec4 1)
	(aref vec4 2)))

(defun vec3->vec4 (vec3 &optional (w 0.0))
  "Adding a 0.0 w component yields a vec4."
  (vec4 (aref vec3 0)
	(aref vec3 1)
	(aref vec3 2)
	w))


(defun mat4*vec3 (mat4 vec3)
  (vec4->vec3
   (matrix*vec4 mat4
		(vec3->vec4 vec3 1.0))))
