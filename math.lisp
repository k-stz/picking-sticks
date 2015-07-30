(defpackage :math
  (:use :cl
	:kit.math)
  (:export :vec2
	   :vec2+
	   :vec2-
	   :vec2*
	   :vec3+))

(in-package :math)

;; alas :kit.math though has a macro to build these automatically, it doesn't export
;; them
(deftype vec2 ()
  `(SIMPLE-ARRAY SINGLE-FLOAT (2)))

(deftype vec3 ()
  `(SIMPLE-ARRAY SINGLE-FLOAT (3)))

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
