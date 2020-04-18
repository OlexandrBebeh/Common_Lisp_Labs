(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(load "func.lisp")
(load "right_join.lisp")

(defun left-join(tab1 tab2 col1 col2 line)
	(cond 
	((= line (length tab1)) #())
	(t (concatenate 'vector 
							(concat-to-table-vectors-left  (aref tab1 line) (filter-inner (aref (aref tab1 line) col1) tab2 col2 1) (length (aref tab2 0))) 
							(left-join tab1 tab2 col1 col2 (+ line 1)) 
		)
	)
)
)


(defun parser-left-join(lst)
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

			(left-join 
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

(defun start-left-join(lst)
	(cond
		((null lst) nil)
		((string-equal "left" (car lst)) (parser-full-join (split-by-one-equal  (car (cut-list-to-el lst "on")))))
		(t (start-full-join (car lst)))
	)
)

(defun fill-right(val tab col line)
	(cond
		((= line (length tab)) t)
		((and (numberp val) (numberp (aref (aref tab line) col))) 
		(cond
			((= val (aref (aref tab line) col)) nil)
			(t (fill-right val tab col (+ line 1)))
			 )
		)
	((and (stringp val) (stringp (aref (aref tab line) col))) 
		(cond
			((string= val (aref (aref tab line) col)) nil)
			(t (fill-right val tab col (+ line 1)))
			 )
		)
	(t (fill-right val tab col (+ line 1)))
	)
)

(defun add-right-join(tab1 tab2 col1 col2 line)
	(cond
		((= line (length tab2)) #())
		((fill-right (aref (aref tab2 line) col2) tab1 col1 1) (concatenate 'vector 
																					(concat-to-table-vectors-right (aref tab2 line) nil (length (aref tab1 0))) 
																					(add-right-join tab1 tab2 col1 col2 (+ 1 line))))
		(t (add-right-join tab1 tab2 col1 col2 (+ 1 line)))
	)
)

(defun parser-full-join(lst)
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

			(left-join 
				tab1 
				tab2 
				(position-col (car (cdr table_col1)) (aref tab1 0) 0)
				(position-col (car (cdr table_col2)) (aref tab2 0) 0)
				1
			)

			(add-right-join 
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

(defun start-full-join(lst)
	(cond
		((null lst) nil)
		((string-equal "full" (car lst)) (parser-full-join (split-by-one-equal  (car (cut-list-to-el lst "on")))))
		(t (start-full-join (car lst)))
	)
)

;(write (select-create-table '("test.csv.row" "test1.csv.row" "test.csv.pos_x" "test1.csv.pos_x" "test1.csv.col") (start-left-join '("left" "join" "test1.csv" "on" "test.csv.pos_x=test1.csv.pos_x"))))