(defpackage :collision
  (:use :cl
	:kit.glm
	:math)
  (:export :extreme-points-along-direction))

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

(defun sphere-support-point (sphere-origin radius direction)
  "Returns the support point of the sphere at origin, sphere-origin, of the given
radius and direction given. f(d)=O+rd/||d|| where d = direction,  O=origin, r=radius and
||d|| = magnitude of d."
  (vec+ sphere-origin
	(vec/ (vec* direction radius)
	      (vec-length direction))))

(defun polyhedra? (vertices faces edges)
  "Vertices (V), Faces (F) and Edges (E) of a Polyhedron relate according to the 
/Euler formula/: V + F - E = 2."
  (= 2
     (+ vertices
	(- faces edges))))


(defun vec-perp (2d-vector)
  "Returns the CCW perpendicular vector."
  (vec2 (- (aref 2d-vector 1))
	(aref 2d-vector 0)))


(defun dot2d (a b)
  "Calculates 2d dot product"
  (+ (* (aref a 0) (aref b 0))
     (* (aref a 1) (aref b 1))))

;; the Quickhull algorithm needs a procedure calculating the furthest point from an edge
(defun point-farthest-from-edge (A B points-list)
  "Use 2d-vectors A and B to form an edge and return point furthest from said edge (CCW
perpedicular)"
  (let* ((edge (vec2- b a))
	 ;; we will _project (!) the points perpedicular to the AB edge_(!!)
	 ;; this way we have a scale from which we just have to read the extreme value!
	 (edge-perpendicular (vec-perp edge))
	 (max-value most-negative-single-float)
	 (right-most-value most-negative-single-float)
	 (return-value))
    (loop for i in points-list 
       :for d = (dot2d (vec2- i a)
			  edge-perpendicular)
       :for r  = (dot2d (vec2- i a)
			   edge)
	 ;; TODO: rewrite for clarity and style
       :if (or (> d max-value) (and (= d max-value) (> r right-most-value))) :do
	 (setf return-value i
	       max-value d
	       right-most-value r)
	 :finally (return return-value))))

(defun extreme-points-along-direction (dir-vec3 points-list)
  (let ((min-proj +9999.0)
	(max-proj -9999.0)
	minimum
	maximum)
    (loop for point in points-list
       for projection = (dot-product point dir-vec3)
       :do ;; implicit progn:
	 (when (< projection min-proj)
	   (setf min-proj projection)
	   (setf minimum point))
	 (when (> projection max-proj)
	   (setf max-proj projection)
	   (setf maximum point)))
    (values minimum maximum)))


;; sphere collision test
(defclass collision-sphere ()
  ((center-point :type vec3 :initarg :center-point)
   (radius :type single-float :initarg :radius)))

(defmethod print-object ((sphere collision-sphere) stream)
  (declare (type stream stream))
  (with-slots (center-point radius) sphere
    (when (or (not *print-readably*) (not *read-eval*))
      (print-unreadable-object (sphere stream :type t :identity t)
	(format stream
		"~S : ~S"
		center-point radius)))))

(defun test-sphere-sphere (sphere-1 sphere-2)
  "Sphere x Sphere collision test."
  (with-slots ((s1-center center-point) (s1-radius radius)) sphere-1
    (with-slots ((s2-center center-point) (s2-radius radius)) sphere-2
      (let* ((distance (vec3- s1-center s2-center))
	     ;; (dot-product x x) is usually an efficient way
	     ;; to double the distance of a vector, we need double
	     ;; the distance because
	     (2xdistance (dot-product distance distance))
	     (radius-sum (+ s1-radius s2-radius)))
	;; then we don't need to calculate the square-root to compare
	;; the distance!
	(print (list s1-center s2-radius))
	(print (list s2-center s2-radius))
	(<= 2xdistance (* radius-sum radius-sum))))))


;; approximative BV-Sphere, using Ritter's algorithm.

;; straight translation
(defun most-separated-points-on-AABB (points-array)
  "Expects an array of vec3 vectors. Returns the points most separated along a principal
axis (as implied by AABB) in order (values min max) where \"min\" is towards the negative portion
of the axis and \"max\" conversely."
  (let ((minx 0) (maxx 0) (miny 0) (maxy 0) (minz 0) (maxz 0))
    (macrolet ((pt. (points-array-index element-vector-index)
		 `(aref (aref points-array ,points-array-index)
			,element-vector-index))
	       (pt (index)
		 `(aref points-array ,index)))
      ;; Find most extreme points along principal axes
      (loop for i from 1 below (length points-array) :do
	   (when (<  (pt. i 0) (pt. minx 0)) (setf minx i))
	   (when (>  (pt. i 0) (pt. maxx 0)) (setf maxx i))
	   (when (<  (pt. i 1) (pt. miny 1)) (setf miny i))
	   (when (>  (pt. i 1) (pt. maxy 1)) (setf maxy i))
	   (when (>  (pt. i 2) (pt. minz 2)) (setf minz i))
	   (when (<  (pt. i 2) (pt. maxz 2)) (setf maxz i)))

      (format t "x:~a ~a ~%y:~a ~a ~%z:~a ~a ~%" (pt minx) (pt maxx) (pt miny) (pt maxy) (pt minz) (pt maxz))
      (let* ((x-principal (vec3- (pt maxx) (pt minx)))
	     (y-principal (vec3- (pt maxy) (pt miny)))
	     (z-principal (vec3- (pt maxz) (pt minz)))
	     ;; square distances of principal axes distances..
	     (dist2x (dot-product x-principal x-principal))
	     (dist2y (dot-product y-principal y-principal))
	     (dist2z (dot-product z-principal z-principal))

	     ;; to find the most distanct "principal" points
	     (min minx)
	     (max maxx))
	(when (and (> dist2y dist2x) (> dist2y dist2z))
	  (setf max maxy
		min miny))
	(when (and (> dist2z dist2x) (> dist2z dist2y))
	  (setf max maxz)
	  (setf min minz))
	(values (pt max) (pt min))))))

(defun sphere-from-distant-points (points-array)
  "COLLISION-SPHERE from array of vec3 points"
  (let ((sphere (make-instance 'collision-sphere)))
    (multiple-value-bind (min max)
	(most-separated-points-on-aabb points-array)
      (with-slots (center-point radius) sphere
	(setf center-point (vec3* (vec3+ min max)
				  0.5))
	(setf radius
	      (sqrt (dot-product
		     (vec3- max center-point)
		     (vec3- max center-point))))))
    sphere))


(defun sphere-of-sphere-and-pt (sphere point-vec3)
  (with-slots (center-point radius) sphere
    (let* ((d (vec3- point-vec3 center-point))
	  ;; again efficiently dealing with distances by
	  ;; only considering the square distances
	  (dist2 (dot-product d d)))
      ;; when point outside sphere?
      (when (> dist2 (* radius radius))
	(let* ((dist (sqrt dist2))
	       (new-radius (* (+ radius dist) 0.5))
	       (k (/ (- new-radius radius) dist)))
	  (setf radius new-radius)
	  (setf center-point (vec3* d k))))))
  sphere)


(defun ritter-sphere (points-array)
  "Computes an approximate BV sphere (as COLLISION-SPHERE) from a point set using Ritter's
algorithm."
  (let ((sphere
	 (sphere-from-distant-points points-array)))
    (loop for i below (length points-array) do
	 (setf sphere
	       (sphere-of-sphere-and-pt sphere
					(aref points-array i))))
    sphere))


(defun mean (number-list)
  "Return the mean of the point set (expects a list of numbers)"
  (/ (loop for i in number-list sum i)
     (length number-list)))

(defun variance (number-list)
  "Computes the Variance of the point set (expects a list of numbers).
Variance is the average of deviation from the MEAN of a set of points."
  ;; "The average of the square of deviations from the mean"
  ;; (/ ((apply #'+ (mapcar #'square <deviations-from-the-mean)) numbers)
  (let ((u 0.0)
	(s2 0.0)
	(n (length number-list)))
    (setf u
	  (mean number-list))
    (values
     (setf s2
	   (/
	    (loop for i in number-list
	       :summing (* (- i u) (- i u)))
	    n))
     u)))

(defun standard-deviation (number-list)
  (multiple-value-bind (variance mean) (variance number-list)
    (values (sqrt variance)
	    mean)))


;; covariance matrix constructor, straight translation:
(defun covariance-matrix (points-list)
  (let ((1/length (/ (length points-list)))
	(centroid (vec3 0.0))
	(cov (make-array '(3 3)))
	(e00 0.0) (e11 0.0) (e22 0.0)
	(e01 0.0) (e02 0.0) (e12 0.0))


    ;; calculate centroid
    (loop for i in points-list do
	 (setf centroid (vec3+ centroid i)))
    (setf centroid (vec3* centroid 1/length))

    (loop for i in points-list
	 ;; p are the points with the centroid at origin!
       for p = (vec3- i centroid) do
	 (macrolet ((p (subscript subscript-2)
		      `(* (aref p ,subscript)
			  (aref p ,subscript-2))))
	   (incf e00 (p 0 0))
	   (incf e11 (p 1 1))
	   (incf e22 (p 2 2))
	   (incf e00 (p 0 1))
	   (incf e01 (p 0 0))
	   (incf e02 (p 0 2))
	   (incf e12 (p 1 2))))

    ;; "fill in the covariance matrix"
    (setf (aref cov 0 0) (* e00 1/length))
    (setf (aref cov 1 1) (* e11 1/length))
    (setf (aref cov 2 2) (* e22 1/length))
    (setf (aref cov 0 1) (* e01 1/length)) (setf (aref cov 1 0) (* e01 1/length))
    (setf (aref cov 0 2) (* e02 1/length)) (setf (aref cov 2 0) (* e02 1/length))
    (setf (aref cov 1 2) (* e12 1/length)) (setf (aref cov 2 1) (* e12 1/length))


    ;; TODO: remove after debugging
    (macrolet ((a (sub1 sub2)
		  `(aref cov ,sub1 ,sub2)))
      (format t "~&~,3f ~,3f ~,3f~%" (a 0 0) (a 0 1) (a 0 2))
      (format t "~&~,3f ~,3f ~,3f~%" (a 1 0) (a 1 1) (a 1 2))
      (format t "~&~,3f ~,3f ~,3f~%" (a 2 0) (a 2 1) (a 2 2)))
    cov))
