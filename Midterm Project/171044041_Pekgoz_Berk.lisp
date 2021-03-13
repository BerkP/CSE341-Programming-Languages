(defvar facts (list))
(defvar predicates (list))
(defvar queries (list))


(defun read-file-as-string (filename)
	(let ( (str "") )
		(with-open-file (in filename)
			(loop for line = (read-line in nil nil) while line do
				(setq str (concatenate 'string str line))
			)
		)
		str
	)
)

(defun separate-horn-clauses (clauses)
	(let ( (curr (car clauses)) ) 
		(if (not (equal curr nil))
			(let ()
				(cond
					((equal (car curr) nil)  (setq queries (append queries (list curr))))
					((equal (car (cdr curr)) nil)  (setq facts (append facts (list curr))))
					(t (setq predicates (append predicates (list curr))))
				)
				(separate-horn-clauses (cdr clauses))
			)	

		)
	)
)


(defun get-unified-predicate (prdct query)
	(let ( prdctparams pparam qparam )
		(setq prdctparams (nth 1 (car prdct) ))

		(loop for i from 0 to (- (list-length prdctparams) 1) do
			(setq pparam (nth i prdctparams))
			(setq qparam (nth i query))


			(if (not-replacable pparam)
				(if (not (equal pparam qparam))
					(return-from get-unified-predicate nil)
				)
				(if (not-replacable qparam)
					(setq prdct (subst qparam pparam prdct :test #'equal))
					(return-from get-unified-predicate nil)
				)
			)
		)

		prdct
	)
)

(defun check-name-and-length (pred_1 pred_2)
	(let (name_1 name_2 len_1 len_2)
		 (setq name_1 (car pred_1))
		 (setq name_2 (car pred_2))
		 (setq len_1 (length (nth 1 pred_1)))
		 (setq len_2 (length (nth 1 pred_2)))
		 (if (and (string-equal name_1 name_2) (equal len_1 len_2)) 
		 	t
		 	nil
		 )
	)
)


(defun evaluate (query)
	(let (pred unified)

		;; First checks facts.
		(loop for i from 0 to (- (list-length facts) 1) do
			(setq pred (car (nth i facts)))

			(if (check-name-and-length query pred)
				(let ()
					(setq unified (get-unified-predicate (nth i facts) (nth 1 query)))

					(if (not (equal unified nil))
						(return-from evaluate t)
					)
				)
			)
		)

		;; Then checks predicates.
		(loop for i from 0 to (- (list-length predicates) 1) do
			(setq pred (car (nth i predicates)))
			(if (check-name-and-length query pred)
				(let (res)
					(setq unified (get-unified-predicate (nth i predicates) (nth 1 query)))
					(if (not (equal unified nil))
						(setq res (evaluate-predicate-list (nth 1 unified)))
					)
					(if res
						(return-from evaluate res)	
					)
				)
			)
		)
	)
)

(defun evaluate-with-var (query)
	(let (pred unifiedq res (reslist (list)))

		(loop for i from 0 to (- (list-length facts) 1) do
			(setq pred (car (nth i facts)))
			(if (check-name-and-length query pred)
				(let ()
					(setq unifiedq (unify-the-vars (nth i facts) query))
					(setq res (evaluate unifiedq))
					(if res
						(setq reslist (append reslist (list (nth 1 unifiedq))))
					)
				)
			)
		)


		(loop for i from 0 to (- (list-length predicates) 1) do
			(setq pred (car (nth i predicates)))
			(if (check-name-and-length query pred)
				(let ()
					(setq unifiedq (unify-the-vars (nth i predicates) query))
					(setq res (evaluate unifiedq))
					(if res
						(setq reslist (append reslist (list (nth 1 unifiedq))))
					)
				)
			)
		)


		reslist
	)


)

(defun unify-the-vars (prdct query)
	(let ( prdctparams queryparams qparam (reslist (list)) )
		(setq prdctparams (nth 1 (car prdct) ))
		(setq queryparams (nth 1 query ))

		(loop for i from 0 to (- (list-length prdctparams) 1) do
			(setq qparam (nth i queryparams))
			(if (not (not-replacable qparam)) 
				(setq query (subst (nth i prdctparams) qparam query :test #'equal))
			)
			(if (and (not-replacable (nth i prdctparams)) (not-replacable qparam) (not (equal (nth i prdctparams) qparam)))
				(return-from unify-the-vars nil)
			)

		)
		query
	)

)



(defun evaluate-predicate-list (predlist)
	(let ()
		(if (equal predlist nil)
			t
			(let (res nextp)
				(setq res (evaluate (car predlist)))
				(setq next (evaluate-predicate-list (cdr predlist)))
				(return-from evaluate-predicate-list (and res next))
			)
		)
	)
)


(defun not-replacable (elmnt)
	(return-from not-replacable (or (is-object elmnt) (is-numeric elmnt) ))
)


(defun is-object (elmnt)
	(if (stringp elmnt)
		(if (and (> (length elmnt) 0) (lower-case-p (char elmnt 0)))
			t
		)
	)
)


(defun is-var (elmnt)
	(if (stringp elmnt)
		(if (and (> (length elmnt) 0) (upper-case-p (char elmnt 0)))
			t
		)
	)
)


(defun is-numeric (elmnt)
	(if (not (stringp elmnt))
		t
		nil
	)
)

(defun evaluate-all-queries (ostream)
	(let (clauses query res (finallist (list)) )

		(setq clauses (read-file-as-string "input.txt" ))
		(setq clauses (read-from-string clauses))
		(separate-horn-clauses clauses)

		(loop for i from 0 to (- (list-length queries) 1) do
			(setq query (nth i queries))
			(setq res (evaluate-with-var (nth 1 query)))
			(setq finallist (append finallist (list res)))
			(format ostream  "~d ~c" res #\newline  )

		)
		finallist
	)
)


(defun main ()
	(with-open-file (stream  "output.txt" :direction :output)
    	(evaluate-all-queries stream)
    )
)

(main)
