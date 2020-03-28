(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defvar map_zal (simple-table:read-csv #P"map_zal-skl9.csv"))
(defvar mp_assistants (simple-table:read-csv #P"mp-assistants.csv" t))
(defvar plenary_register_mps (simple-table:read-tsv #P"plenary_register_mps-skl9.tsv"))

(defvar test (simple-table:read-csv #P"test.csv"))

(load "func.lisp")
(load "select.lisp")

(defvar key_words '("select" "distinct" "from" "where" "not" "and" "or" "order" "by" "asc" "desc"))

(defun del-empty(lst)
	(cond
		((null lst) nil)
		((string-equal (car lst) "") (del-empty (cdr lst)))
		(t (list* (car lst) (del-empty (cdr lst))))
	))

(defun group-by(lst)
(cond
((null lst) nil)
((string-intersection (car lst) key_words) (list* (car lst) (group-by (cdr lst))))
(t (cond
		((string-intersection (car (cdr lst)) key_words) (list* (car lst) (group-by (cdr lst))))
		(t (list* (concat (car lst) (car (cdr lst))) (group-by (cdr (cdr lst)))))
		))
)
)

(defun parser-comand(str)
(group-by (group-by (group-by
(del-empty (split-by-one-space str))
)))
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
(defun inquiry-to-db(str)
	(let ((listCommand (parser-comand str)))
		(cond
			((string-equal (car listCommand)  "SELECT") (select-inquiry  (cdr listCommand)))
			(t (pprint "Error"))
		)
	))


(defun execute-command(str)
	(let ((command (parse-command str)))
	(cond
	  ((string-equal command "exit") (exit))
	  ((string-equal command "inquiry") (inquiry-to-db (cut-parameter str)))
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

;(write (parser-comand "SELECT DISTINCT col1    ,  col2      , col3   FROM   tab   WHERE    col1   = 10 AND col2 = Sir. Dit Senior OR NOT col1 < 10 "))
(write (execute-command "inquiry(SELECt * from map_zal-skl9.csv order by col)"))
;(pprint (simple-table:read-csv #P"mp-posts_full.csv"))

