;; GTU CSE331 Programming Languages
;; Berk Pekgoz
;; --------------------------------



;; Struct for Huffman Tree node.
(defstruct node
	data
	freq
	left
	right
)

;; Initial function for reading frequencies from file.
;; Returns a node list which has all characters in paragraph with frequencies.
(defun get-frequencies-from-file (filename)
	(let (nodelist)
		(with-open-file (stream filename)
			(setq nodelist (get-nodelist-from-stream stream)))
		nodelist)
)

;; Function to read file character by character.
;; This function use array to keep frequencies and then converts it to a node list.
(defun get-nodelist-from-stream (stream)
	(let ( (charArr (make-array '(256) :initial-element 0)) currChar index)

		(setq currChar (read-char stream nil nil nil))

		(loop while (not (null currChar)) do
			(setq index (char-code currChar))
			(if (< index 256)
				(setf (aref charArr index) (+ (aref charArr index) 1)))

			(setq currChar (read-char stream nil nil nil)))
		(convert-array-to-nodelist charArr)
	)
)

;; Function to convert a array to a node list.
;; Index of array shows code of character and the value in that index shows frequency.
(defun convert-array-to-nodelist (arr)
	(let (nodelist currFreq)
		(loop for index from 0 to 255 do

			(setq currFreq (aref arr index))
			(if (> currFreq 0)
				(push (create-node index (aref arr index)) nodelist))	
		)
		nodelist
	)
)

;; Creates new node with specifed data.
;; Node-Data is code/charcode
;; Node-Freq is freq
(defun create-node (code freq)
	(let (newNode currChar)
		(setq currChar (code-char code))
		(setq newNode (make-node :data currChar
			:freq freq
			:left nil
			:right nil))
	)
)

;; A recursive function to get node with minimum frequency in the node list
(defun get-min-freq-node (nodelist &optional (minnode (car nodelist)) )
	(if (null nodelist)
		minnode
		(if (< (node-freq (car nodelist)) (node-freq minnode))
			(get-min-freq-node (cdr nodelist) (car nodelist))
			(get-min-freq-node (cdr nodelist) minnode)))
)

;; Removes specified node from node list.
;; Uses delete method with equal test key.
;; Delete method cannot remove first element. 
;; So if the specfied node is the first node, returns cdr List.
(defun remove-node (node nodelist)
	(let ()
		(delete node nodelist :test #'equal ) 
		(if (equal node (car nodelist))
			(cdr nodelist)
			nodelist)
	)
)


;; Builds huffman tree from a node list.
;; Main algorithm: Get min 2 node and combine. 
;; Then push the new node to the list until one node left.
;; Returns node list with one node which is the root of the huffman tree.
(defun build-huffman (nodelist)
	(loop while (not (null (cdr nodelist)))
		do (let (min1 min2 intNode)

			(setq min1 (get-min-freq-node nodelist))
			(setq nodelist (remove-node min1 nodelist))

			(setq min2 (get-min-freq-node nodelist))
			(setq nodelist (remove-node min2 nodelist))

			(setq intNode (make-node :data nil
				:freq (+ (node-freq min1) (node-freq min2))
				:left min1 :right min2))
			(push intNode nodelist)
		)
	)
	nodelist
)

;; Initial function to write tree to file.
(defun write-codes-to-file (hufftree name)
	(with-open-file (stream  name :direction :output )
  	(traverse-the-tree hufftree stream))
)

;; Traverses tree level by levelt until helper function return nil.
;; Calls helper function for each level to traverse
(defun traverse-the-tree (hufftree &optional (stream nil) )
	(let ( (levelcount 0) (flag t))
		(loop while (eql flag t) do 
			(setq flag (traverse-by-level hufftree levelcount stream))
			(setq levelcount (+ levelcount 1))
		)
	)
)

;; Goes until the specified level on the tree.
;; If theres is no that level at a branch returns nil else T.
(defun traverse-by-level (localnode level stream &optional (huffcode nil))
	(if (null localnode)
		nil
		(let ( (currdata (node-data localnode)) )
			(if (= level 0) 
				(if (not (null currdata))
					(write-level-to-file currdata huffcode stream) t)
				(get-next-level localnode (- level 1) stream huffcode))
		)
	)
)

;; Function to make a recursive call for traverse-by-level function.
;; Sets new parameters for next recursive call.
(defun get-next-level (node level stream code)
	(let (leftcode rightcode leftres rightres)
		(setq leftcode  (concatenate 'string code "0"))
		(setq rightcode (concatenate 'string code "1"))
		(setq leftres  (traverse-by-level (node-left node) level stream leftcode))
		(setq rightres (traverse-by-level (node-right node) level stream rightcode))
		(or leftres rightres)
	)
)

;; Writes character and code to specfied stream.
;; Example "e: 000".
(defun write-level-to-file (data code stream)
	(format stream  "~c: " data  )
	(write-line code stream)
	(return-from write-level-to-file t))

;; Main function to call functions step by step
(defun main ()
	(setq nodelist (get-frequencies-from-file "paragraph.txt"))
	(setq nodelist (build-huffman nodelist))
	(write-codes-to-file (car nodelist) "huffman_codes.txt")
)


;; Calls main function.
(main)



