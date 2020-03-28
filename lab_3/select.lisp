(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(load "func.lisp")
(load "where.lisp")
(load "order-by.lisp")

(defvar test (simple-table:read-csv #P"test.csv"))


(defun create-list-of-collumns(vec lst)
(cond 
((null lst) nil)
((string-intersection (car lst) (vector-to-list vec 0)) (list* (position-col (car lst) vec 0) (create-list-of-collumns vec (cdr lst))))
(t (create-list-of-collumns vec (cdr lst)))
)
)

(defun select-all(lst1 lst2 n)
	(cond
		((null lst1) nil)
		((string= (car lst1) "*") (append (reverse (generation n)) (select-all (cdr lst1) lst2 n)))
		(t (list* (car lst2) (select-all (cdr lst1) (cdr lst2) n)))
	)
)
(defun get-list-of-collumns(vec lst)
(select-all lst (create-list-of-collumns vec lst) (length vec))
)



(defun select-create-table(cols table)
	(let ((listOfCol (get-list-of-collumns (aref table 0) cols)))
		(cond
			((null listOfCol) nil)
			(t (simple-table:select1 table listOfCol))
		))
)

(defun chek-from(cols lst)
(cond
((string-equal "FROM" (car lst))  (select-create-table cols (where-command (cut-list-to-el lst "where") (load-table (car (cut-list-to-el lst "from"))))))
(t (pprint "FROM not found"))
)
)


(defun collum-of-table-to-list(table n)
	(cond 
	((= n (length table)) nil)
	(t (list* (aref (aref table n) 0) (collum-of-table-to-list table (+ n 1))))
	)
)

(defun make-distinct-list(lst table n)
(cond
	((null lst) nil)
	((stringp (car lst)) (cond
				((string= (car lst) (aref (aref table n) 0)) (list* n (make-distinct-list (cdr lst) table (+ n 1))))
				(t (make-distinct-list lst table  (+ n 1)))
				))
	((numberp (car lst))(cond
				((= (car lst) (aref (aref table n) 0)) (list* n (make-distinct-list (cdr lst) table (+ n 1))))
				(t (make-distinct-list lst table (+ n 1)))
				))
	(t (make-distinct-list lst table (+ n 1)))
)
)

(defun build-distincts-list(cols table)
	(make-distinct-list
		(collum-of-table-to-list (simple-table:distinct table (car (get-list-of-collumns (aref table 0) cols))) 0)
		(simple-table:select table (car (get-list-of-collumns (aref table 0) cols)))
		0
	)
)

(defun get-distinct-rows(lst table)
	(cond 
		((null lst) (simple-table:make-table))
		(t (simple-table:add-to-table (aref table (car lst)) (get-distinct-rows (cdr lst) table) ))
	)
)

(defun select-distinct(lst)
(let ((table (chek-from (split-by-one-comma (car lst)) (cdr lst))))
	(get-distinct-rows
		(reverse (build-distincts-list (split-by-one-comma (car lst)) table))
		table)
)
)

(defun select-inquiry(lst)
(cond
((string-equal (car lst) "distinct") (order-by (cut-list-to-el lst "by") (select-distinct (get-list-before-el (cdr lst) "order"))))
(t (order-by (cut-list-to-el lst "by")  (chek-from (split-by-one-comma (car lst)) (get-list-before-el (cdr lst) "order"))))
)
)



;(write (select-inquiry '("distinct" "row,col,col" "from" "map_zal-skl9.csv" "where" "row=2")))

;(write (simple-table:distinct (load-table "map_zal-skl9.csv") 0))
;(write (build-distincts-list '("row") '("from" "map_zal-skl9.csv")))
;(write (build-distinct '("20") '("from" "test.csv")))
;(write (simple-table:read-csv (car '("test.csv"))))
;(write (get-list-of-collumns (simple-table:get-row 0 test) '("20" "30")))

;(write ((string-intersection "20" (vector-to-list (simple-table:get-row 0 test) 0))))

;(write (coerce (aref (simple-table:get-row 0 test) 6) 'character))
