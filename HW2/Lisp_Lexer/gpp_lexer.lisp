;;--------------------
;;Berk PEKGOZ
;;171044041
;;CSE 341 Assignment_1
;;--------------------



(defvar Keys (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" 
					"set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(defvar Ops (list "+" "-" "/" "**" "*" "(" ")" "\"" "\"" ",") )


(defvar KeyNames (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" 
						"KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(defvar OpNames (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_DBLMULT" "OP_MULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA") )




(defvar op_c_status nil)





(defun tokenize-the-string (str ostream)
	(let ( start_i curr_i sstr strlen printed)

		(setq strlen (length str))
		(setq start_i 0)

		(loop for curr_i from 1 to strlen do

			(setq sstr (subseq str start_i curr_i))
			(setq sstr (string-downcase sstr))
			(setq printed nil)
			

			(if (and (string-equal sstr ";")  (< curr_i strlen )  (string-equal (char str curr_i) ";") )
				(return-from tokenize-the-string 99)
			)

			(if (equal (is-string-a-identifier sstr) t)
				(let ()
					(if (or (and (< curr_i strlen) (not (null (is-in-the-list ops (char str curr_i))))) (= curr_i strlen))
						(let ( (key_no (is-in-the-list Keys sstr)) )
							(if (null key_no)
								(write-line "IDENTIFIER" ostream)
								(write-line (nth  key_no KeyNames) ostream)

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
						(let () (write-line "VALUE" ostream) (setq start_i curr_i) (setq printed t))	
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

						(write-line (nth  op_no OpNames) ostream)
						(setq start_i curr_i)
						(setq printed t)
					)
				)
			)

			(if (and (or (= curr_i strlen) (not (null (is-in-the-list ops (char str curr_i))))) (equal printed nil))
				(let ()
					(write-line (concatenate 'string "ERROR " sstr " cannot be tokenized!") ostream)
					(setq start_i curr_i)
				)
			)		
		)
	)
)

(defun tokenize-the-line (line ostream)

	(let (strList  done feedback)
		(setq strList (line-to-strlist line))

		(loop while (and (not (null (car strList))) (null done)) do
			
			(setq feedback (tokenize-the-string (car strList) ostream))
			(setq strList (cdr strList))

			(if (equal feedback 99)
				(let () (write-line "COMMENT" ostream) (setq done t))
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

(defun from-console (ostream)
	(let (done line)

		(loop while (null done) do 
			(setq line (read-line))
			(if (string-equal line "")
				(setq done t)
				(tokenize-the-line line ostream)
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
					(tokenize-the-line line ostream)
					(setq line (read-line istream nil))
				)
			)
		)
	)
)

(defun lexer (ostream)
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
    	(lexer stream)
    )
)

(main)
