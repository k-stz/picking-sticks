(defpackage :game-collision
  (:use :cl
	:kit.math
	:game-objects)
  (:export :collision?))

(in-package :game-collision)


(defgeneric collision? (obj obj))

(defmethod collision? (o1 o2)
  (format t "no collision test for object class: ~%~a ~a" (class-of o1) (class-of o2))
  nil)

(defmethod collision? ((r1 rectangle) (r2 rectangle))
  (test-aabb-aabb r1 r2))

(defun test-aabb-aabb (r1 r2)
  "Perform AABB collision test."
  (macrolet ((v (vec3 subscript)
	       `(aref ,vec3 ,subscript)))
    (with-slots ((a.c center-point) (a.r radius)) (bounding-volume r1)
      (with-slots ((b.c center-point) (b.r radius)) r2
	(cond ((> (abs (- (v a.c 0) (v b.c 0)))
		  (abs (+ (v a.r 0) (v b.r 0))))
	       nil)
	      ((> (abs (- (v a.c 1) (v b.c 1)))
		  (abs (+ (v a.r 1) (v b.r 1))))
	       nil)
	      ;; perform z-test last, as the rectangles mainly
	      ;; vary in their x,y values
	      ((> (abs (- (v a.c 2) (v b.c 2)))
		  (abs (+ (v a.r 2) (v b.r 2))))
	       nil)
	      (t ;; all exclusionary conditionas failed i.e. the rectangles
	       t ;; collide
	       ))))))

