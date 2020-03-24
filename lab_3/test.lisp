(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defvar map_zal (simple-table:read-csv #P"map_zal-skl9.csv"))
(defvar mp_assistants (simple-table:read-csv #P"mp-assistants.csv" t))
(defvar plenary_register_mps (simple-table:read-tsv #P"plenary_register_mps-skl9.tsv"))

(defvar test (simple-table:read-csv #P"test.csv"))

(load "func.lisp")
(load "select.lisp")



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
(defun inquiry-to-db(str)
	(cond
		((string-equal (car (split-by-one-space (cut-parameter str)))  "SELECT") (select-inquiry  (cdr (split-by-one-space (cut-parameter str)))))
		(t (pprint "Error"))
	)
)


(defun execute-command(str)
	(let ((command (parse-command str)))
	(cond
	  ((string-equal command "exit") (exit))
	  ((string-equal command "inquiry") (inquiry-to-db str))
	  ((string-equal command "load") (load-table (cut-parameter str)))
	  ((string-equal command "show") (read-file (cut-parameter str)))
	  (t (pprint "Error: entered command not fund!!!"))
	  )
	)
)


(defun start-run ()
    (loop
    (terpri)
    (princ "[user]:")
    (terpri)
    (execute-command (read-line))
    )
)


(write (execute-command "inquiry(SELECt distinct row,* from map_zal-skl9.csv)"))
;(pprint (simple-table:read-csv #P"mp-posts_full.csv"))

