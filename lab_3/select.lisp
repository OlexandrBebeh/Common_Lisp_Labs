(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(load "func.lisp")

(defvar test (simple-table:read-csv #P"test.csv"))

(defun create-list-of-collumns(vec lst n)
(cond 
((= (length vec) n) nil)
((string-intersection (aref vec n) lst) (list* n (create-list-of-collumns vec lst (+ n 1))))
(t (create-list-of-collumns vec lst (+ n 1)))
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
(select-all lst (create-list-of-collumns vec lst 0) (length vec))
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
((string-equal "FROM" (car lst)) (select-create-table cols (load-table (car (cdr lst)) )))
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

(defun build-distincts-list(cols lst)
	(make-distinct-list
		(collum-of-table-to-list (simple-table:distinct (load-table (car (cdr lst))) (car (get-list-of-collumns (aref (load-table (car (cdr lst))) 0) cols))) 0)
		(simple-table:select (load-table (car (cdr lst))) (car (get-list-of-collumns (aref (load-table (car (cdr lst))) 0) cols)))
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
(get-distinct-rows
	(reverse (build-distincts-list (split-by-one-comma (car lst)) (cdr lst)))
	(chek-from (split-by-one-comma (car lst)) (cdr lst))
)
)

(defun select-inquiry(lst)
(cond
((string-equal (car lst) "distinct") (select-distinct (cdr lst)))
(t (chek-from (split-by-one-comma (car lst)) (cdr lst)))
)
)

;(write )
;(write (simple-table:distinct (load-table "map_zal-skl9.csv") 0))
;(write (build-distincts-list '("row") '("from" "map_zal-skl9.csv")))
;(write (build-distinct '("20") '("from" "test.csv")))
;(write (simple-table:read-csv (car '("test.csv"))))
;(write (get-list-of-collumns (simple-table:get-row 0 test) '("20" "30")))

;(write ((string-intersection "20" (vector-to-list (simple-table:get-row 0 test) 0))))

;(write (coerce (aref (simple-table:get-row 0 test) 6) 'character))
