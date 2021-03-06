(load "day08.lisp")

;; TESTS
(format t "TESTS~%")
(print (parse-instruction "nop +0"))
(print (parse-instruction "acc -99"))
(print (gethash 6 (read-program "sample.txt")))
(format t "~%")


(defun bbb ()
  (let* ((dict (make-hash-table))
        (pr (gethash 'a dict)))
    (setf (gethash 'a dict) 7)
    (setf (gethash 'b dict) 3)
    (if (nth-value 1 pr)
        3
        4)))
(print (bbb))

;; (load "tests.lisp")
