(load "cli.lisp")

(execute-command "inquiry(SELECt test.csv.row, test.csv.col,test.csv.pos_x, test.csv.pos_y, test1.csv.row,test1.csv.col, test1.csv.pos_x,test1.csv.pos_y from test.csv left join test1.csv on test.csv.col = test1.csv.col where test1.csv.col > 20 order by test1.csv.col desc)")

(execute-command "inquiry(SELECt test.csv.row, test.csv.row, avg(test1.csv.col), count(test1.csv.col) from test.csv full join test1.csv on test.csv.row = test1.csv.row group by test.csv.row having count(test1.csv.col) = 1 order by count(test1.csv.col) desc)")

(execute-command "inquiry(SELECt *, , , from test1.csv where not row = 2 order by col)")

(execute-command "inquiry(select row, max(col), avg(pos_x), count(title) from map_zal-skl9.csv group by row having row = 10)")









(defun parse-as(lst)
	(
		((null lst) "")
		(t (concatenate 'string (car lst)))
	)
)



(defun case-inquiry-to-db(listCommand)
		(cond
			((string-intersection "group" listCommand) (start-group-by (cdr listCommand)))
			((find_join listCommand) (select-join (cdr listCommand)))
			(t (select-inquiry  (cdr listCommand)))
		)
)


(defun take-case-ex(lst)
	(cond
		((null lst) nil)
		((string-equal "case" (car lst)) (get-list-before-el lst "AS"))
		(t (take-case-ex (cdr lst)))
	)
)
(defun all-except-case(lst)
	(append 
		(get-except-last (get-list-before-el lst "case"))
		(list (concatenate 'string (get-last (get-list-before-el lst "case")) (car (cdr (cut-list-to-el lst "as")))) )
		(cdr (cdr (cut-list-to-el lst "as"))))
)
(defun start-case(lst)
	(
		print-table (case-inquiry-to-db (all-except-case lst))
	)
)
