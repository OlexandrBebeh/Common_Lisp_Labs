(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)


(defvar test (simple-table:read-csv #P"test.csv"))

(load "group-by.lisp")
(load "func.lisp")


(defvar key_words '("select" "distinct" "from" "where" "not" "and" "or" "order" "by" "asc" "desc" "inner" "join" "on" "full" "outer" "left" "right" "on" "group" "by" "having"))

(defvar join_words '("inner" "join" "on" "full" "outer" "left" "right" "on" "union"))

(defun find_join(lst)
(cond 
((null lst) nil)
((string-intersection (car lst) join_words) t)
(t (defun find_join (cdr lst)))
)
)

(defun del-empty(lst)
	(cond
		((null lst) nil)
		((string-equal (car lst) "") (del-empty (cdr lst)))
		(t (list* (car lst) (del-empty (cdr lst))))
	))

(defun parse-list(lst)
(cond
	((null (cdr lst)) lst)
	((null lst) nil)
	((string-intersection (car lst) key_words) (list* (car lst) (parse-list (cdr lst))))
	(t (cond
			((string-intersection (car (cdr lst)) key_words) (list* (car lst) (parse-list (cdr lst)) ))
			(t (parse-list (list* (concat (car lst) (car (cdr lst))) (cdr (cdr lst))))  )
			))
)
)

(defun parser-comand(str)
(parse-list
(del-empty (split-by-one-space str))
)
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
			((and (string-equal (car listCommand)  "SELECT") (string-intersection "group" listCommand)) (print-table (start-group-by (cdr listCommand))))
			((and (string-equal (car listCommand)  "SELECT") (find_join listCommand)) (print-table (select-join (cdr listCommand))))
			((string-equal (car listCommand)  "SELECT") (print-table (select-inquiry  (cdr listCommand))))
			(t (print listCommand))
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

;(execute-command "inquiry(SELECt test.csv.col   , test1.csv.col   , test.csv.pos_x, test1.csv.title from test.csv left join test1.csv on test.csv.col = test1.csv.col where test1.csv.col > 20 order by test1.csv.col desc)")
;(execute-command "inquiry(SELECt test.csv.row, test.csv.row, avg(test1.csv.col), count(test1.csv.col) from test.csv full join test1.csv on test.csv.row = test1.csv.row group by test.csv.row having count(test1.csv.col) = 1 order by count(test1.csv.col) desc)")
;(write (parser-comand "SELECT DISTINCT col1    ,  col2      , col3   FROM   tab   WHERE    col1   = 10 AND col2 = Sir. Dit Senior OR NOT col1 < 10 "))
;(write (execute-command "inquiry(SELECt * from test1.csv order by col)"))
;(pprint (simple-table:read-csv #P"mp-posts_full.csv"))
;(start-run)
(execute-command "inquiry(select row, max(col), avg(pos_x), count(title) from map_zal-skl9.csv group by row having row = 10)")
;(execute-command "inquiry(SELECt * from map_zal-skl9.csv)")
;(execute-command "inquiry(SELECt distinct Avg(col),Max(pos_x)   ,Count(pos_y) from map_zal-skl9.csv)")
