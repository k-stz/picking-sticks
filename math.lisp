(defpackage :math
  (:use :cl
	:kit.math)
  (:export :foo))

(in-package :math)

(defun vec2+ (v1 v2)
  (vec2 (+ (aref v1 0) (aref v2 0))
	(+ (aref v1 1) (aref v2 1))))


(defun vec2- (v1 v2)
  (vec2 (- (aref v1 0) (aref v2 0))
	(- (aref v1 1) (aref v2 1))))


(defun vec2* (v1 scalar)
  (vec2 (* (aref v1 0) scalar)
	(* (aref v1 1) scalar)))


