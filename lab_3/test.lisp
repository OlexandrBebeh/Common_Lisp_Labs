(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defvar map_zal (simple-table:read-csv #P"map_zal-skl9.csv"))
(defvar mp_assistants (simple-table:read-csv #P"mp-assistants.csv" t))
(defvar plenary_register_mps (simple-table:read-tsv #P"plenary_register_mps-skl9.tsv"))

(defvar test (simple-table:read-csv #P"test.csv"))





(defun split-by-one-dot (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\. string :start i)
          collect (subseq string i j)
          while j))


(defun get-last(lst)
	(cond 
		((null (cdr lst)) (car lst))
		(t (get-last (cdr lst)))
		)
)



(defun load-table(tableName)
	(cond
		((string= tableName "map_zal-skl9.csv") (pprint map_zal))
		((string= tableName "mp-assistants.csv") (pprint mp_assistants))
		((string= tableName "plenary_register_mps-skl9.tsv") (plenary_register_mps))
		((string= (get-last (split-by-one-dot tableName)) "csv") (simple-table:read-csv tableName))
		((string= (get-last (split-by-one-dot tableName)) "tsv") (simple-table:read-tsv tableName))
		(t (print "file type not define"))
)
)

(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))


(defun split-by-one-comma(string)
    (loop for i = 0 then (1+ j)
          as j = (position #\, string :start i)
          collect (subseq string i j)
          while j))


(defun read-file(nameFile)
(let ((in (open nameFile :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
         while line do (format t "~a~%" line))
    (close in)))
)

(defun inquiry-to-db(str)
	(cond
		((string-equal (car (split-by-one-space str))  "SELECT") (select-inquiry  (cdr (split-by-one-space str))))
		(t (pprint "command not found;("))
	)
)



(defun cut-parameter (command)
	  (subseq command (+ (position #\( command) 1) (position #\) command :from-end t))
  )

(defun parse-command (commandQuery)
  (let ((openBracketPosition (position #\( commandQuery)))
	(setq openBracketPosition (cond
					((not openBracketPosition) 0)
					(t openBracketPosition)
					))
	(subseq commandQuery 0 openBracketPosition)
	)
)


(defun execute-command(str)
	(let ((command (parse-command str)))
	(cond
	  ((string= command "exit") (exit))
	  ((string= command "inquiry") (inquiry-to-db (cut-parameter str)))
	  ((string= command "load") (load-table (cut-parameter str)))
	  ((string= command "show") (read-file (cut-parameter str)))
	  (t (pprint "Error: entered command not fund!!!"))
	  )
	)
)


(defun start-run ()
    (loop
    (terpri)
    (princ "[user]:")
    (terpri)
    (if (string= (execute-command (read-line)) "EXIT")
      (return)
      ()
      )
    )
)

(pprint (simple-table:read-csv #P"mp-posts_full.csv"))

