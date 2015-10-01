(defpackage :game-collision
  (:use :cl
	:game-objects))

(in-package :game-collision)


(defgeneric collision? (obj obj))

(defmethod collision? (o1 o2)
  (format t "no collision test for object class: ~%~a ~a" (class-of o1) (class-of o2))
  nil)

(defmethod collision? ((r1 rectangle) (r2 rectangle))
  (list r1 r2))
