(defpackage :math
  (:use :cl
	:kit.math)
  (:export :vec2
	   :vec2+
	   :vec2-
	   :vec2*
	   :vec3+
	   :vec3-
	   :vec3*))

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

