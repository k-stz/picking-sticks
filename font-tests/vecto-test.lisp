(defpackage #:vecto-examples
  (:use #:cl #:vecto))

(in-package #:vecto-examples)

(defun radiant-lambda (file)
  (with-canvas (:width 90 :height 90)
    (let ((font (get-font "/home/k-stz/quicklisp/local-projects/picking-sticks/font-tests/times.ttf"))
          (step (/ pi 7)))
      (set-font font 40)
      (translate 45 45)
      (draw-centered-string 0 -10 #(#x3BB))
      (set-rgb-stroke 1 0 0)
      (centered-circle-path 0 0 35)
      (stroke)
      (set-rgba-stroke 0 0 1.0 0.5)
      (set-line-width 4)
      (dotimes (i 14)
        (with-graphics-state
          (rotate (* i step))
          (move-to 30 0)
          (line-to 40 0)
          (stroke)))
      (save-png file))))

(radiant-lambda "/home/k-stz/quicklisp/local-projects/picking-sticks/font-tests/TEST-PIC.png")
