;; GTU CSE331 Programming Languages
;; Berk Pekgoz
;; --------------------------------


;; Function to check if number is prime or not.
;; If number is higher than 1, calls helper function.
(defun is-prime (n) 
	(if (< n 2) nil
		(prime n (- n 1)))
)

;; Helper function to determine number is prime or not recursively
(defun prime (n d ) 
 	(if (= d 1) t
  		(if (= (rem n d) 0) nil
  			(prime n (- d 1))))
)

;; Function to check if number is semi prime or not.
;; If number is higher than 1, calls helper function.
(defun is-semi-prime (n)
	(if (< n 2) nil
		(if (> (semi-prime n (- n 1) 0) 2 ) nil t))
)

;; Helper function to determine number is semi-prime or not recursively
;; Uses is-prime function as helper function.
(defun semi-prime ( n d cnt)
	(if (= d 1) cnt 
		(cond 
			((and (= (rem n d) 0) (is-prime d)) (semi-prime n (- d 1) (+ cnt 1)))
			((= (rem n d) 0) (semi-prime n (- d 1) (+ cnt 3)))
			(t (semi-prime n (- d 1) cnt))
		))
)

;; Reads numbers from file
(defun get-file (filename)
	(with-open-file (stream filename)
		(read-line stream nil))
)


;; Writes content to file
(defun write-to-file (name str)
  	(with-open-file (stream  name :direction :output)
  	(format stream str))
)


;; Returns output string.
;; Loop goes lower bound to upper bound and for each number checks for prime and semi prime.
;; If one of them 
(defun get-output-string (l-bound u-bound)
	(let ((str nil))
		(loop for a from l-bound to u-bound do
			(cond
				((eql (is-prime a) t) (setq str (concatenate 'string  str (format nil "~S is Prime~C" a #\Newline ))))
				((eql (is-semi-prime a) t) (setq str (concatenate 'string  str (format nil "~S is Semi-prime~C" a #\Newline))))
			)
		)
		str
	)
)

;; Main function to call functions step by step
(defun prime-crawler (inputFile outputFile)
	(setq bounds (get-file inputFile))
	(setq bounds (read-from-string (concatenate 'string "(" bounds ")")))
	(setq l-bound (car bounds))
	(setq u-bound (car (cdr bounds)))
	(write-to-file outputFile (get-output-string l-bound u-bound))
)


(prime-crawler "boundries.txt" "primedistribution.txt")
