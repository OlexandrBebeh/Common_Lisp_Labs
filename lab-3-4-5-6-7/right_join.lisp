(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)


(load "func.lisp")
(load "inner_join.lisp")

(defun right-join(tab1 tab2 col1 col2 line)
	(cond 
	((= line (length tab2)) #())
	(t (concatenate 'vector 
		(concat-to-table-vectors-right  (aref tab2 line) (filter-inner (aref (aref tab2 line) col2) tab1 col1 1) (length (aref tab1 0))) 
		(right-join tab1 tab2 col1 col2 (+ line 1)) 
		)
	)
)
)


(defun parser-right-join(lst)
	(let (
		(table_col1 (split-by-last-dot (car lst)))
		(table_col2 (split-by-last-dot (car (cdr lst))))
		)
	(let (
			(tab1 (load-table (car table_col1)))
			(tab2 (load-table (car table_col2)))
		)
		( concatenate 'vector 

			(vector (concatenate 'vector 
							(make-collumn-names (aref tab1 0) (car table_col1) 0)
							(make-collumn-names (aref tab2 0) (car table_col2) 0)
						))

			(right-join 
				tab1 
				tab2 
				(position-col (car (cdr table_col1)) (aref tab1 0) 0)
				(position-col (car (cdr table_col2)) (aref tab2 0) 0)
				1
			)
		)
	)
	)
)

(defun start-right-join(lst)
	(cond
		((null lst) nil)
		((string-equal "right" (car lst)) (parser-right-join (split-by-one-equal  (car (cut-list-to-el lst "on")))))
		(t (start-right-join (car lst)))
	)
)


;(print-table (order-by '("test.csv.row") (select-create-table  
;	'("test.csv.row" "test.csv.pos_x" "test.csv.pos_y" "test1.csv.row" "test1.csv.pos_x" "test1.csv.pos_y" )  
;	(start-right-join '("right" "join" "test1.csv" "on" "test.csv.pos_x=test1.csv.pos_x")))
;		))