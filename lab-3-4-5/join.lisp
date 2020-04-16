(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(load "func.lisp")
(load "full_join.lisp")

(defun distinct-join(lst)
(let ((table (select-create-table (split-by-one-comma (car lst)) (where-command (cut-list-to-el lst "where") (first-join lst)))))
	(get-distinct-rows
		(reverse (build-distincts-list (split-by-one-comma (car lst)) table))
		table)
)
)

(defun select-join(lst)
	(cond 
		((and (string-equal (car lst) "distinct") (chekc-function (car (cdr lst)))) (db-func (split-by-one-comma (car (cdr lst))) (distinct-join (list "*" (cdr (cdr lst))))))
		((chekc-function (car lst)) (db-func (split-by-one-comma (car lst)) (select-create-table '("*") (where-command (cut-list-to-el lst "where") (first-join lst)))))
		((string-equal "distinct" (car lst)) (order-by (cut-list-to-el lst "by") (distinct-join (cdr lst))))
		(t	(order-by (cut-list-to-el lst "by") 
										(select-create-table (split-by-one-comma (car lst)) (where-command (cut-list-to-el lst "where") (first-join lst)))) )
	)
)

(defun first-join(lst)
	(cond
		((null lst) nil)
		((string-equal "full" (car lst)) (start-full-join lst))
		((string-equal "inner" (car lst)) (start-inner-join lst))
		((string-equal "right" (car lst)) (start-right-join lst))
		((string-equal "left" (car lst)) (start-left-join lst))
		(t (first-join (cdr lst)))
	)
)

;(write (select-join '("test.csv.pos_x,test1.csv.pos_x" "from" "test.csv" "left" "join" "test1.csv" "on" "test.csv.pos_x=test1.csv.pos_x")))
;(print-table (select-join '("Max(test.csv.title),Count(test1.csv.title),Avg(test.csv.pos_x)" "from" "test.csv" "full" "outer" "join" "test1.csv" "on" "test.csv.pos_x=test1.csv.pos_x")))