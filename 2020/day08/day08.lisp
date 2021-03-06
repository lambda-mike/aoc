;; 
;; Advent of Code 2020 Day08
;; 

;; string -> [String, Int]
(defun parse-instruction (line)
  (let ((op (subseq line 0 3))
        (n (nth-value 0 (parse-integer (subseq line 3)))))
    (list op n)))

;; string -> Map[Int, [String, Int]]
(defun read-program (file)
  (with-open-file (stream file)
    (let ((i 0)
          (instructions (make-hash-table)))
    (loop for line = (read-line stream nil)
          while line do
            (setf (gethash i instructions) (parse-instruction line))
            (incf i))
      instructions)))

;; Int -> String -> Int -> Int
(defun update-pointer (&key p op n)
  (if (string= "jmp" op)
      (+ n p)
      (+ 1 p)))

;; Int -> String -> Int -> Int
(defun update-acc (&key acc op n)
  (if (string= "acc" op)
      (+ n acc)
      acc))

;; Map[Int, [String, Int]] (Int Map[Int, nil] Int) -> Int
;; where [String, Int] is a pair of operation and int
(defun solve-a (program &key (pointer 0) (log (make-hash-table)) (acc 0))
  (let* ((duplicate (nth-value 1 (gethash pointer log)))
         (instr (nth-value 0 (gethash pointer program)))
         (terminate (not instr))
         (op (first instr))
         (n (second instr)))
    (setf (gethash pointer log) nil)
    (if (or duplicate terminate)
        (values acc terminate)
        (solve-a
         program
         :pointer (update-pointer :p pointer :op op :n n) 
         :log log
         :acc (update-acc :acc acc :op op :n n)))))

;; String -> String
(defun inverse-op (op)
  (if (string= "jmp" op)
      "nop"
      (if (string= "nop" op)
          "jmp"
          op)))

;; Map[Int, [String, Int]] -> Int
(defun solve-b (program)
  (with-hash-table-iterator (iter program)
    (loop
      (multiple-value-bind (entry-p pointer instr)
          (iter)
        ;; acc and termination result
        (multiple-value-bind (acc ok)
            (let* ((op (first instr))
                   (n (second instr))
                   (new-op (inverse-op op)))
              ;; toggle op at line n
              (setf (gethash pointer program) (list new-op n))
              (solve-a program))
          (when (and entry-p ok) (return acc))
          ;; toggle back to instr
          (setf (gethash pointer program) instr))))))

(defun main ()
  ;; (let* ((file "sample.txt")
  (let* ((file "input.txt")
         (program (read-program file)))
    ;; 2014
    (format t "Solving Day08A...~%")
    (format t "~a~%" (solve-a program))
    ;; 2251
    (format t "Solving Day08B...~%")
    (format t "~a~%" (solve-b program))))

(main)
