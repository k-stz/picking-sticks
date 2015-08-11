(defpackage :collision
  (:use :cl
	:kit.glm))

(in-package :collision)

(defun orient2d (A B C)
  "Takes 2d points a,b and c. If returned value:
> 0.0: C lies left to directed line AB; triangle ABC is CCW
< 0.0: C lies to the right of AB; triangle is CW.
= 0.0: A B C are collinear.
The returned value is also the signed are of the triangle*2"
  (declare (simple-array a b c))
  (macrolet ((v (vec2 subscript)
	       `(aref ,vec2 ,subscript)))
    (matrix-determinant
     (matrix (v a 0) (v a 1) 1.0 0.0
	     (v b 0) (v b 1) 1.0 0.0
	     (v c 0) (v c 1) 1.0 0.0
	     0.0     0.0     0.0 1.0))))
