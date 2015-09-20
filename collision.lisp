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

(defun orient3d (A B C D)
  "Takes 3d points A, B, C and D. If returned value:
Above the plane means that viewed from D the triangle appears to be CCW.
> 0.0: D is above the ABC plane (the plane supporting triangle ABC)
< 0.0: D below or \"D sees a CW triangle\"
= 0.0: D is coplanar with ABC.
The returned value is also the signed are of the tetrahedron*6"
  (declare (simple-array a b c))
  (macrolet ((v (vec2 subscript)
	       `(aref ,vec2 ,subscript)))
    (matrix-determinant
     (matrix (v a 0) (v a 1) (v a 2) 1.0
	     (v b 0) (v b 1) (v b 2) 1.0
	     (v c 0) (v c 1) (v c 2) 1.0
	     (v d 0) (v d 1) (v d 2) 1.0))))


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

(defun mat4->mat3 (mat4)
  (let ((mat4s-inner-mat3
	 (loop for i below (- (length mat4) 4)
	    unless (= 0 (mod (1+ i) 4))
	    collecting (aref mat4 i))))
    (make-array 9 :element-type 'single-float
		:initial-contents mat4s-inner-mat3)))


(defun 3x3-det (mat3x3)
  "Calculate determinant of row-major matrix"
  (matrix-determinant (mat4->mat3 mat3x3)))


;; TODO: this returns double the area; see ORIENT2D
(defun tri-area-2d (x1 y1 x2 y2 x3 y3)
  (- (* (- x1 x2)
	(- y2 y3))
     (* (- x2 x3)
	(- y1 y2))))

;; straightforward translation from "Real-time Collision Detection"
(defun barycentric (A B C P)
  "Returns the barycentric coordinates of triangle ABC in respect to point P in order: (u, v, w)
Where: P = uA + vB + wC"
  (let* (;; Unnormalized triangle normal
	 (m (cross-product (vec- B A) (vec- C A)))
	 ;; Nominators and one-over-denminator for u and v ratios
	 nu nv ood
	 ;; Absolute components for determining projection plane
	 (x (abs (aref m 0)))
	 (y (abs (aref m 1)))
	 (z (abs (aref m 2))))
    (cond ((and (>= x y)
		(>= x z))
	   (setf nu (tri-area-2d (aref p 1) (aref p 2) (aref b 1) (aref b 2) (aref c 1) (aref c 2)))
	   (setf nv (tri-area-2d (aref p 1) (aref p 2) (aref c 1) (aref c 2) (aref a 1) (aref a 2)))
	   (setf ood (/ (aref m 0))))
	  ;; else if
	  ((and (>= y x)
		(>= y z))
	   (setf nu (tri-area-2d (aref p 0) (aref p 2) (aref b 0) (aref b 2) (aref c 0) (aref c 2)))
	   (setf nv (tri-area-2d (aref p 0) (aref p 2) (aref c 0) (aref c 2) (aref a 0) (aref a 2)))
	   (setf ood (/ (- (aref m 1)))))
	  ;; else
	  (t
	   (setf nu (tri-area-2d (aref p 0) (aref p 1) (aref b 0) (aref b 1) (aref c 0) (aref c 1)))
	   (setf nv (tri-area-2d (aref p 0) (aref p 1) (aref c 0) (aref c 1) (aref a 0) (aref a 1)))
	   (setf ood (/ (aref m 2)))))
    (let (u v w)
      (setf u (* nu ood)
	    v (* nv ood)
	    w (- 1.0 u v))
      (values u v w))))

(defun inside-triangle? (P A B C)
  "Tests _containment_ of a Point P in a triangle ABC."
  (multiple-value-bind (u v w) (barycentric A B C P)
    (declare (ignore u))
    (and (>= v 0.0)
	 (>= w 0.0)
	 (<= (+ v w) 1.0))))


;; Planes

(defun make-plane (a b c)
  "Expects points A, B and C to be CCW and returns a plane structure using the
constant-normal form n⋅P = d or ax+by+cz-d=0"
;; Builds a plane using the constant normal form: n⋅P = d, where d is (dot-product n P),
;; where P is a given point on the plane.
  (let* ((n (normalize (cross-product (vec- b a) (vec- c a))))
	 (d (dot-product n a)))
    (vector n d)))

(defun plane-normal (plane)
  (aref plane 0))

(defun plane-d (plane)
  (aref plane 1))


(defun is-quad-convex? (a b c d)
  "The quad is convex if:
  (BD x BA)⋅(BD x BC) < 0 and
  (AC x AD)⋅(AC x AB) < 0"
  (let ((BD (vec- D B)) (BA (vec- A B)) (BC (vec- C B))
	(AC (vec- C A)) (AD (vec- D A)) (AB (vec- B A)))
    (if (and (< (dot-product (cross-product bd ba)
			     (cross-product bd bc))
		0)
	     (< (dot-product (cross-product ac ad)
			     (cross-product ac ab))
		0))
	;; convex
	t
	;; else non-convex
	nil
	)))
