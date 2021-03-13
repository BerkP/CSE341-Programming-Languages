;; GTU CSE331 Programming Languages
;; Berk Pekgoz
;; --------------------------------


;; Function to convert nested list to flattened list.
(defun make-flattened (mylist)
    (if ( not (null mylist))
        (if (atom (car mylist))

            (prog ((temp nil) (nested nil))
                (setq temp (make-flattened (cdr mylist)))

                (if (null temp)
                    (setq nested mylist)
                    (setq nested (push (car mylist) temp)))

                (return nested))
            (append (make-flattened (car mylist)) (make-flattened (cdr mylist))))
        nil
    )
)

;; Reads file and returns content
(defun get-file (filename)
    (with-open-file (stream filename)
        (read-line stream nil)))

;; COnverts a list to string without parentheses
(defun list-to-string (mylist)
    (if (null mylist)
        nil
        (concatenate 'string (format nil "~S " (car mylist)) (list-to-string (cdr mylist)))))

;; Writes content to file
;; File name specified with parameter
(defun write-to-file (name str)
    (with-open-file (stream  name :direction :output)
    (format stream str))name)


;; Main function to call functions step by step
(defun main ()
    (setq nestedList (get-file "nested_list.txt"))
    (setq nestedList (read-from-string (concatenate 'string "(" nestedList ")")))
    (setq flattenedList (make-flattened nestedList) )
    (write-to-file "flattened_list.txt" (list-to-string flattenedList))
)

;; Starts program
(main)


