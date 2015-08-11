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


(defun 2x2-det (mat2x2)
  "Calculate determinant of row-major matrix"
  (let ((a (aref mat2x2 0))
	(b (aref mat2x2 1))
	(c (aref mat2x2 2))
	(d (aref mat2x2 2)))
    (- (* a d)
       (* b c))))

;; from arcsynthesis' glm
(defun mat3->-mat4 (mat3)
  "Put mat3 into top-left corner of an identity mat4"
  (make-array 16 :element-type 'single-float
	      :initial-contents
	      (append 
	       (loop for i across mat3
		  for x = 1 then (1+ x)
		  if (= (mod x 3) 0)
		  collect i and collect 0.0
		  else collect i)
	       '(0.0 0.0 0.0 1.0))))

(defun 3x3-det (mat3x3)
  "Calculate determinant of row-major matrix"
  (matrix-determinant (mat4->mat3 mat3x3)))
