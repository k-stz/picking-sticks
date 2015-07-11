;; experimental, DO NOT LOAD


;; from random-state.net
(defmacro with-foo ((foo) &body body)
  `(call-with-foo (lambda (,foo) ,@body)))

(defun call-with-foo (function)
  (let (foo)
    (unwind-protect
        (funcall function (setf foo (get-foo)))
      (when foo (release-foo foo)))))


(defmacro do-seq-hash ((key value seq-hash &optional result) &body body)
  ;; finally worked just putting ,seq-hash and ,result outside the body
  ;; not making them part of the (lambda ...,@body) looping part, because
  ;; they're part of the scaffolding to build the looping part!!
  `(call-do-seq-hash ,seq-hash (lambda (,key value) ,@body) ,result))


;; BOOOOOM works!!!!
(defun call-do-seq-hash (seq-hash function result)
  (progn (loop for key across (keys-in-order seq-hash) 
	     for value = (gethash key (the-table seq-hash)) do
	       (funcall function key value))
	  result))


;; expands:

(defmacro do-seq-hash ((key value seq-hash &optional result) &body body)
  `(call-do-seq-hash (lambda (,key ,value ,seq-hash &optional ,result)  ,@body)))

(CALL-DO-SEQ-HASH
 (LAMBDA (K V *DYNAMIC-RECTANGLES* &OPTIONAL ()) (PRINT (LIST K V))))


(defmacro do-upto-x ((i x) &body body)
  `(loop for ,i upto ,x do (progn ,@body)))


(defmacro do-upto-x ((i x) &body body)
  `(call-do-upto-x ,x (lambda (,i) ,@body)))

(defun call-do-upto-x (x function)
  (loop for i upto x do
	(funcall function i)))

;; from #lisp
;; > The CALL-WITH-... or INVOKE-WITH-... style is mostly used when
;;   some dynamic context is established for the duration of the execution
;;   of some BODY forms, and then removed.

;; > Open a file, execute forms, close the file.  Change the
;;   foreground color, execute some forms, change it back to what it was.
;;   Etc.  [15:27]


(call-for-i-upto-x x fun) =>
(defmacro for-i-upto-x ((i x)&body body)
  `(call-for-i-upto-x ,x (lambda (,i) ,@body))))))
       approximately ; ^ !   

(defun call-for-i-upto-x (x fun)
  (dotimes (i x) (funcall fun i)))

