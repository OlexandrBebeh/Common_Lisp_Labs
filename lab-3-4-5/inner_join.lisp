(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defvar test (simple-table:read-csv #P"test.csv"))

(load "func.lisp")
(load "select.lisp")

(defun filter-inner(val tab col line)
(cond 
	((= line (length tab)) nil)
	((and (numberp val) (numberp (aref (aref tab line) col))) 
		(cond
			((= val (aref (aref tab line) col)) (LIST* (aref tab line) (filter-inner val tab col (+ line 1))))
			(t (filter-inner val tab col (+ line 1)))
			 )
		)
	((and (stringp val) (stringp (aref (aref tab line) col))) 
		(cond
			((string= val (aref (aref tab line) col)) (LIST* (aref tab line) (filter-inner val tab col (+ line 1))))
			(t (filter-inner val tab col (+ line 1)))
			 )
		)
	(t (filter-inner val tab col (+ line 1)))
))


(defun inner-join(tab1 tab2 col1 col2 line)
(cond 
	((= line (length tab1)) #())
	(t (concatenate 'vector 
		(concat-to-table-vectors (aref tab1 line) (filter-inner (aref (aref tab1 line) col1) tab2 col2 1)) 
		(inner-join tab1 tab2 col1 col2 (+ line 1))
		)
	)
)
)

(defun parser-inner-join(lst)
	(let (
		(table_col1 (split-by-last-dot (car lst)))
		(table_col2 (split-by-last-dot (car (cdr lst))))
		)
	(let ((tab1 (load-table (car table_col1)))
			(tab2 (load-table (car table_col2))))
		( concatenate 'vector 

			(vector (concatenate 'vector 
							(make-collumn-names (aref tab1 0) (car table_col1) 0)
							(make-collumn-names (aref tab2 0) (car table_col2) 0)
						))

			(inner-join 
				tab1 
				tab2 
				(position-col (car (cdr table_col1)) (aref tab1 0) 0)
				(position-col (car (cdr table_col2)) (aref tab2 0) 0)
				0
			)
		)
	)
	)
)


(defun start-inner-join(lst)
(cond 
	((null lst) nil)
	((string-equal "inner" (car lst)) (parser-inner-join (split-by-one-equal  (car (cut-list-to-el lst "on")))))
	(t (start-inner-join (car lst)))

))

;(print-table (select-create-table '("test.csv.row" "test1.csv.row" "test.csv.col" "test.csv.id_mp") (start-inner-join '("inner" "join" "test1.csv" "on" "test.csv.row=test1.csv.row"))))