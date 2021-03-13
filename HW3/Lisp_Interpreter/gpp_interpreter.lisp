;;--------------------
;;Berk PEKGOZ
;;171044041
;;CSE 341 Assignment_2
;;--------------------



(defvar Keys (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" 
					"set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(defvar Ops (list "+" "-" "/" "**" "*" "(" ")" "\"" "\"" ",") )


(defvar KeyNames (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" 
						"KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(defvar OpNames (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_DBLMULT" "OP_MULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA") )


(defvar op_c_status nil)


(defvar lexed-tokens (list))
(defvar lexed-str (list))


(defvar id-strings (list))
(defvar id-values (list))




(defun tokenize-the-string (str ostream)
	(let ( start_i curr_i sstr strlen printed)

		(setq strlen (length str))
		(setq start_i 0)

		(loop for curr_i from 1 to strlen do

			(setq sstr (subseq str start_i curr_i))
			(setq sstr (string-downcase sstr))
			(setq printed nil)
			

			(if (and (string-equal sstr ";")  (< curr_i strlen )  (string-equal (char str curr_i) ";") )
				(let ()
					(setq lexed-tokens (append lexed-tokens (list "COMMENT")))
					(setq lexed-str (append lexed-str (list "COMMENT")))
					(return-from tokenize-the-string 99)
				)
				
			)

			(if (equal (is-string-a-identifier sstr) t)
				(let ()
					(if (or (and (< curr_i strlen) (not (null (is-in-the-list ops (char str curr_i))))) (= curr_i strlen))
						(let ( (key_no (is-in-the-list Keys sstr)) )
							(if (null key_no)
								(let () (setq lexed-tokens (append lexed-tokens (list "IDENTIFIER"))) (setq lexed-str (append lexed-str (list sstr))))
								(let () (setq lexed-tokens (append lexed-tokens (list (nth  key_no KeyNames)))) (setq lexed-str (append lexed-str (list (nth  key_no KeyNames)))))
							)
							(setq start_i curr_i)
							(setq printed t)

						)
					)
				)
			)

			(if (equal (is-string-a-value sstr) t)
				(let ()
					(if (or (and (< curr_i strlen) (not (null (is-in-the-list ops (char str curr_i))))) (= curr_i strlen))
						(let () (setq lexed-tokens (append lexed-tokens (list "VALUE"))) (setq lexed-str (append lexed-str (list sstr))) (setq start_i curr_i) (setq printed t))	
					)
				)
			)	


			(let ( (op_no (is-in-the-list Ops sstr))  ) 
				(if (null op_no)
					nil

					(let ()
						(if (string-equal sstr "\"")
							(setq op_no (oc-or-cc))
						)

						(if (and (string-equal sstr "*") (< curr_i strlen )  (string-equal (char str curr_i) "*"))
							(let () (setq op_no 3) (setq curr_i (+ curr_i 1)) )		;; 3 = value of DB_MULT
						)

						(let () (setq lexed-tokens (append lexed-tokens (list (nth  op_no OpNames)))) (setq lexed-str (append lexed-str (list (nth  op_no OpNames)))))
						(setq start_i curr_i)
						(setq printed t)
					)
				)
			)

			(if (and (or (= curr_i strlen) (not (null (is-in-the-list ops (char str curr_i))))) (equal printed nil))
				(let ()
					(write-line (concatenate 'string "SYNTAX ERROR " sstr " cannot be tokenized!") ostream)
					(setq start_i curr_i)
					(return-from tokenize-the-string 98)
				)
			)		
		)
	)
)





(defun tokenize-the-line (line ostream)

	(let (strList  done feedback)
		(setq strList (line-to-strlist line))
		(setq lexed-tokens (list))
		(setq lexed-str (list))

		(loop while (and (not (null (car strList))) (null done)) do
			
			(setq feedback (tokenize-the-string (car strList) ostream))
			(setq strList (cdr strList))

			(if (equal feedback 99)
				(let () 
					(write-line "SYNTAX OK! Result: COMMENT" ostream) 
					(setq done t)
					(return-from tokenize-the-line 97)
				)
			)
			(if (equal feedback 98)
				(let ()  
					(setq done t)
					(return-from tokenize-the-line 97)
				)
			)
		)
	)
)





(defun oc-or-cc ()
	(let (r) 

		(if (equal op_c_status nil)
			(setq r 7)  ;; value of op_oc
			(setq r 8)	;; value of op_cc
		)

		(if (= r 7)
			(setq op_c_status t)
			(setq op_c_status nil)
		)
		r
	)
)





(defun is-string-a-value (str)
	(prog ( firstChar strlen (isValue t) )
		(setq firstChar (char str 0))
		(setq strlen (length str))

		(if (and (> strlen 1) (char= firstChar #\0 ) )
			(setq isValue nil)
		)

		(if  (and (eql isValue t) (equal (every #'digit-char-p str) nil) )
			(setq isValue nil)
		)

		(return isValue)
	)
)





(defun is-string-a-identifier (str)
	(prog ((c (char str 0)))

		(if (not (or (char= c #\_ ) (alpha-char-p c )))
			(return-from is-string-a-identifier nil)
		)

		(loop for i from 1 to (- (length str) 1) do
			(setq c (char str i))
			(if (not (or (char= c #\_) (alpha-char-p c) (digit-char-p c) ))
				(return-from is-string-a-identifier nil)
			)
		)
		(return t)
	)
)






(defun is-in-the-list (strList targetStr &optional (index 0))
	(let ((currStr (car strList)))
		(if (equal currStr nil)
			nil
			(if (string-equal targetStr currStr)
				index
				(is-in-the-list (cdr strList) targetStr (+ index 1))
			)
		)
	)
)






(defun line-to-strlist (line)

	(let ( (linelen (length line)) (i 0) curr_i (strList (list)) str chr)
		(loop for curr_i from 1 to linelen do
			(setq chr (subseq line i curr_i))

			(if (or (string= chr #\Newline) (string= chr #\Space) (string= chr #\Tab) )
				(if (not (null str)) 
					(let () (push str strList) (setq str nil))
				)
				(setq str (concatenate 'string str chr))
			)
			(setq i (+ i 1))
		)
		(if (not (null str)) (push str strList) )
		(reverse strList)
	)
)







(defun expi (tokenlist strlist isInput)

	(let ( curr currStr curr_i expsize exp_end_i value_1 value_2 (output nil) (llen (list-length tokenlist) ) )

		(if (and (string-equal (car tokenlist) "OP_OP") (string-equal (nth (- llen 1) tokenlist) "OP_CP" ))

			(let ()
				(setq curr_i 1)
				(setq curr (nth curr_i tokenlist))

				(if (or (string-equal curr "OP_PLUS") (string-equal curr "OP_MINUS") (string-equal curr "OP_DIV") (string-equal curr "OP_MULT") (string-equal curr "OP_DBLMULT")) 
					(let ()
						;; Getting first value
						(setq curr_i (+ curr_i 1))
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						;;is expression of first value valid
						(if (equal expsize -1) (return-from expi nil))
						(setq exp_end_i (+ curr_i expsize))
						(setq value_1 
							(expi (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i) 0))
						;;is first value valid
						(if (null value_1) (return-from expi nil))
						;;Getting second value
						(setq curr_i exp_end_i)
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						;;is expression of second value valid
						(if (equal expsize -1) (return-from expi nil))
						(setq exp_end_i (+ curr_i expsize))
						(setq value_2 
							(expi (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i) 0))
						;;is second value valid
						(if (or (not (string-equal "OP_CP" (nth exp_end_i tokenlist))) (null value_2)) (return-from expi nil))

						(if (string-equal curr "OP_PLUS") 		(setq output (+ value_1 value_2)))
						(if (string-equal curr "OP_MINUS") 		(setq output (- value_1 value_2)))
						(if (string-equal curr "OP_MULT") 		(setq output (* value_1 value_2)))
						(if (string-equal curr "OP_DIV") 		(setq output (/ value_1 value_2)))
						(if (string-equal curr "OP_DBLMULT")	(setq output (expt value_1 value_2)))
					)
				)


				(if (and (string-equal curr "KW_SET") (string-equal (nth (+ curr_i 1) tokenlist) "IDENTIFIER") ) 
					(let ()

						(setq currStr (nth (+ curr_i 1) strlist))

						(setq curr_i (+ curr_i 2))
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						
						(if (equal expsize -1) (return-from expi nil))

						(setq exp_end_i (+ curr_i expsize))
						(setq value_1 
							(expi (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i) 0))
						
						(if (null value_1) (return-from expi nil))
						(if (or (not (string-equal "OP_CP" (nth exp_end_i tokenlist)))) (return-from expi nil))

						(set-id currStr value_1)

						(setq output value_1)
					)
				)


				(if (string-equal curr "KW_IF")
					(let ()
						
						(setq curr_i (+ curr_i 1))
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						
						(if (equal expsize -1) (return-from expi nil))
						(setq exp_end_i (+ curr_i expsize))
						(setq value_1 
							(expb (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i)))
						
						(if (null value_1) (return-from expi nil))
						
						(setq curr_i exp_end_i)
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						
						(if (equal expsize -1) (return-from expi nil))
						(setq exp_end_i (+ curr_i expsize))
						(setq value_2 
							(expi (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i) 0))
						
						(if (or (not (string-equal "OP_CP" (nth exp_end_i tokenlist))) (null value_2)) (return-from expi nil))

						(if (equal value_1 1)
							(setq output value_2)
							(setq output 0)
						)
					)
				)



			)

		)

		(if (and (equal llen 1) (string-equal (car tokenlist) "VALUE") )
			(setq output (parse-integer (car strlist)))
		)

		(if (and (equal llen 1) (string-equal (car tokenlist) "IDENTIFIER") )
			(setq output (get-id (car strlist)))
		)
		output
	)
)




(defun expb (tokenlist strlist)

	(let ( curr curr_i expsize exp_end_i value_1 value_2 (output nil) (eqflag 0) (llen (list-length tokenlist) ) )

		(if (and (string-equal (car tokenlist) "OP_OP") (string-equal (nth (- llen 1) tokenlist) "OP_CP" ))
			(let ()
				(setq curr_i 1)
				(setq curr (nth curr_i tokenlist))

				(if (string-equal curr "KW_EQUAL")
					(let ()
						
						(setq curr_i (+ curr_i 1))
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						
						(if (equal expsize -1) (setq eqflag 1))
						(if (not (equal eqflag 1))
							(let ()(setq exp_end_i (+ curr_i expsize))
							(setq value_1 
								(expi (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i) 0))
							
							(if (null value_1) (setq eqflag 1)))
						)
						(if (not (equal eqflag 1))
							
							(let ()(setq curr_i exp_end_i)
							(setq expsize (get-expression-size (subseq tokenlist curr_i)))
							
							(if (equal expsize -1) (setq eqflag 1)))
						)
						(if (not (equal eqflag 1))
							(let ()
								(setq exp_end_i (+ curr_i expsize))
								(setq value_2 
									(expi (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i) 0)
							)
							
							(if (or (not (string-equal "OP_CP" (nth exp_end_i tokenlist))) (null value_2)) (setq eqflag 1)))
						)
						(if (not (equal eqflag 1)) (if (equal value_1 value_2) (setq output 1) (setq output 0)) (setq curr_i 1))
					)
				)
				(if (or (string-equal curr "KW_AND") (string-equal curr "KW_OR") (and (string-equal curr "KW_EQUAL") (equal eqflag 1)))
					(let ()
						
						(setq curr_i (+ curr_i 1))
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						
						(if (equal expsize -1) (return-from expb nil))
						(setq exp_end_i (+ curr_i expsize))
						(setq value_1 
							(expb (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i)))
						
						(if (null value_1) (return-from expb nil))
						
						(setq curr_i exp_end_i)
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						
						(if (equal expsize -1) (return-from expb nil))
						(setq exp_end_i (+ curr_i expsize))
						(setq value_2 
							(expb (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i)))
						
						(if (or (not (string-equal "OP_CP" (nth exp_end_i tokenlist))) (null value_2)) (return-from expb nil))

						(if (string-equal curr "KW_AND") 		(setq output (bin-and value_1 value_2)))
						(if (string-equal curr "KW_OR") 		(setq output (bin-or value_1 value_2)))
						(if (string-equal curr "KW_EQUAL") 		(setq output (bin-equal value_1 value_2)))
					)
				)

				(if (string-equal curr "KW_NOT")
					(let ()
						;; Getting first value
						(setq curr_i (+ curr_i 1))
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						;;is expression of first value valid
						(if (equal expsize -1) (return-from expb nil))
						(setq exp_end_i (+ curr_i expsize))
						(setq value_1 
							(expb (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i)))
						;;is first value valid
						(if (or (not (string-equal "OP_CP" (nth exp_end_i tokenlist))) (null value_1)) (return-from expb nil))

						(if (equal value_1 1) (setq output 0) (setq output 1))	
					)
				)
			)
		)
		(if (and (equal llen 1) (or  (string-equal (car tokenlist) "KW_TRUE") (and (string-equal (car tokenlist) "VALUE") (equal (car strlist) "1")) ) )
			(setq output 1))
		(if (and (equal llen 1) (or  (string-equal (car tokenlist) "KW_FALSE") (and (string-equal (car tokenlist) "VALUE") (equal (car strlist) "0")) ) )
			(setq output 0))
		output
	)
)






(defun explisti (tokenlist strlist isInput)

	(let ( curr curr_i expsize exp_end_i value_1 value_2 (output (list)) (llen (list-length tokenlist) ) )

		(if (and (string-equal (car tokenlist) "OP_OP") (string-equal (nth (- llen 1) tokenlist) "OP_CP" ))

			(let ()
				(setq curr_i 1)
				(setq curr (nth curr_i tokenlist))

				(if (string-equal curr "KW_LIST")
					(setq output (get-list tokenlist strlist))
				)

				(if (or (string-equal curr "KW_CONCAT") (string-equal curr "KW_APPEND"))
					(let ()
						
						(setq curr_i (+ curr_i 1))
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						
						(if (equal expsize -1) (return-from explisti nil))
						(setq exp_end_i (+ curr_i expsize))

						(if (string-equal curr "KW_CONCAT")
							(setq value_1 
								(explisti (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i) 0))
							(setq value_1 
								(expi (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i) 0))
						)
						
						(if (null value_1) (return-from explisti nil))
						
						(setq curr_i exp_end_i)
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						
						(if (equal expsize -1) (return-from explisti nil))
						(setq exp_end_i (+ curr_i expsize))
						(setq value_2 
							(explisti (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i) 0))
						
						(if (or (not (string-equal "OP_CP" (nth exp_end_i tokenlist))) (null value_2)) (return-from explisti nil))


						(if (string-equal curr "KW_APPEND")
							(setq value_1 (list value_1))
						)

						(setq output (append output value_1))
						(setq output (append output value_2))
						output	
					)
				)


				(if (string-equal curr "KW_IF")
					(let ()
						
						(setq curr_i (+ curr_i 1))
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						
						(if (equal expsize -1) (return-from explisti nil))
						(setq exp_end_i (+ curr_i expsize))
						(setq value_1 
							(expb (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i)))
						
						(if (null value_1) (return-from explisti nil))
						
						(setq curr_i exp_end_i)
						(setq expsize (get-expression-size (subseq tokenlist curr_i)))
						
						(if (equal expsize -1) (return-from explisti nil))
						(setq exp_end_i (+ curr_i expsize))
						(setq value_2 
							(explisti (subseq tokenlist curr_i exp_end_i) (subseq strlist curr_i exp_end_i) 0))
						
						(if (or (not (string-equal "OP_CP" (nth exp_end_i tokenlist))) (null value_2)) (return-from explisti nil))

						(if (equal value_1 1)
							(setq output value_2)
							(setq output (list 0))
						)
					)
				)

			)
		)




		output
	)
)




(defun get-expression-size (tokenlist)

	(let ( (size 1) (count 1) )

		(if (not (string-equal (car tokenlist) "OP_OP"))
			size
			(let ()
				(loop for tkn in (cdr tokenlist) do
					(if (string-equal tkn "OP_OP") (setq count (+ count 1)))
					(if (string-equal tkn "OP_CP") (setq count (- count 1)))
					(setq size (+ size 1))
					(if (equal count 0) (return-from get-expression-size size))
				)
				-1
			)
		)
	)
)




(defun get-list (tokenlist strlist)
	(let ( (the-list (list)) (len (list-length tokenlist)) temp)
		(loop for i from 2 to (- len 2) do
			(setq temp (parse-integer (nth i strlist)))
			(if (string-equal (nth i tokenlist) "VALUE")
				(setq the-list (append the-list (list temp)))
				(return-from get-list nil)
			)
		)
		the-list
	)
)





(defun set-id (idstr value)
	(let (index)
		(setq index (position idstr id-strings :test #'string-equal))
		(if (null index)
			(let () 
				(setq id-strings (append (list idstr) id-strings)) 
				(setq id-values (append (list value) id-values))
			)
			(setf (elt id-values index) value)
		)
	)
)




(defun get-id (idstr)
	(let (index)
		(setq index (position idstr id-strings :test #'string-equal))
		(if (null index)
			0
			(nth index id-values)
		)
	)
)





(defun bin-equal 	(v1 v2) (if (equal v1 v2) 1 0))

(defun bin-or 		(v1 v2) (if (or (= v1 1) (= v2 1)) 1 0))

(defun bin-and 		(v1 v2)	(if (and (= v1 1) (= v2 1)) 1 0))




(defun parse-the-line (line ostream)
	(let (feedback)
		(setq feedback (tokenize-the-line line ostream))
		(if (not (equal feedback 97))
			(let (res)
				(setq res (expi lexed-tokens lexed-str 1))
				(if (equal res nil)
					(setq res (explisti lexed-tokens lexed-str 1))
				)

				(if (not (equal res nil))
					(format ostream  "SYNTAX OK! Result: ~d ~c" res #\newline  )
					(write-line "SYNTAX ERROR! Expression is not recognized!" ostream)
				)
			)


		)

	)

)


(defun from-console (ostream)
	(let (done line)
		(loop while (null done) do 
			(setq line (read-line))
			(if (string-equal line "")
				(setq done t)
				(parse-the-line line ostream)
			)
		)
	)
)

(defun from-file (filename ostream)
	(let (istream)
		(setq istream (open filename))
		(if (not (null istream))
			(let (line)
				(setq line (read-line istream nil))
				(loop while (not (null line)) do
					(parse-the-line line ostream)
					(setq line (read-line istream nil))
				)
			)
		)
	)
)

(defun parser (ostream)
	(let (filename)
		(setq filename (car *args*))
		(if (null filename)
			(from-console ostream)
			(from-file filename ostream)
		)
	)
)

(defun main ()
	(with-open-file (stream  "parsed_lisp.txt" :direction :output)
    	(parser stream)
    )
)

(main)