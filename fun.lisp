(defpackage :fun
  (:use :cl))

(in-package :fun)


(defun power-consumption ()
  (format t  "~&How much do you weigh? (kg)~%")
  (let ((kg (read)))
    (format t "~&You need ~akj to survive the next 24hours!"
	    (floor (* kg 0.08 60.0 24)))))


(defun bmi (kg meters)
  "Calculates the body mass index (BMI)."
  (let ((bmi
	 (/ kg
	    (expt meters 2))))
    (if (and (< bmi 25) (> bmi 18.5))
	(format t "~&Your bmi is ~5f Just fine!  ~%" bmi)
	(format t "~&Your bmi is ~5f Oh, oh ~%" bmi))))
