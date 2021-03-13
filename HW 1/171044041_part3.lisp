;; GTU CSE331 Programming Languages
;; Berk Pekgoz
;; --------------------------------


;; Reads integers from file adds them to a list
;; Max 5 integer.
;; Calls helper function.
(defun read-file-for-ints (filename)
	(let (intlist)
		(with-open-file (stream filename)
			(setq intlist (get-integers-from-file stream)))
		intlist
	)
)


;; Helper function to read integers from stream object.
;; Max 5 integer.
(defun get-integers-from-file (stream) 
	(let (intlist  int (counter 0))
		(setq int (read stream nil nil nil))
		(loop while (and (not(null int)) (< counter 5)) do
			(push int intlist)
			(setq int (read stream nil nil nil))
			(setq counter (+ counter 1))
		)
		(reverse intlist)
	)
)

;; Writes content to file
;; File name specified with parameter
(defun write-sequence-to-file (name nlist)
    (with-open-file (stream  name :direction :output)
    	(write-list-to-collatz-sequences nlist stream))
)


;; Writes collatz sequences for each element in the list
(defun write-list-to-collatz-sequences (nlist stream)
	(setq number (car nlist))
	(if (not (null number))
		(let ()
			(princ (concatenate 'string (write-to-string number) ": ") stream)
			(if (= number 0)
				(princ "Invalid!" stream)
				(write-collatz-sequence number stream)
			)
			(princ #\Newline stream)
			(write-list-to-collatz-sequences (cdr nlist) stream)
		)
	)
)

;; Writes collatz sequence for a number
(defun write-collatz-sequence (n stream)
	(princ (concatenate 'string (write-to-string n) " ") stream)
	(if (not (= n 1))
		(if (= (rem n 2) 0)
			(write-collatz-sequence (/ n 2) stream)
			(write-collatz-sequence (+ (* n 3) 1) stream)
		)
	)
)

;; Main function to call functions step by step
(defun main()
	(setq nlist (read-file-for-ints "integer_inputs.txt"))
	(write-sequence-to-file "collatz_outputs.txt" nlist)
)


(main)