(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(load "func.lisp")

(defun get-rows-from-el(dit table)
	(cond 
		((= dit (length table)) (simple-table:make-table))
		(t (simple-table:add-to-table (aref table dit) (get-rows-from-el (+ dit 1) table) ))
	)
)

(defun asc(x y col)
(cond
((null (cdr col)) (cond 
					((and (stringp (aref x (car col))) (stringp (aref y (car col)))) (string< (aref x (car col)) (aref y (car col))))
					((and (numberp (aref x (car col))) (numberp (aref y (car col)))) (< (aref x (car col)) (aref y (car col))))
					((equal (aref x (car col)) "") nil)
					(t t)))
((and (stringp (aref x (car col))) (stringp (aref y (car col)))) (cond 
																		((string= (aref x (car col)) (aref y (car col))) (asc x y (cdr col)))
																		(t (string< (aref x (car col)) (aref y (car col))))))
((and (numberp (aref x (car col))) (numberp (aref y (car col)))) (cond 
																		((= (aref x (car col)) (aref y (car col))) (asc x y (cdr col)))
																		(t (< (aref x (car col)) (aref y (car col))))))
((equal (aref x (car col)) "") nil)
(t t)
)
)

(defun desc(x y col)
(cond
((null (cdr col)) (cond 
					((and (stringp (aref x (car col))) (stringp (aref y (car col)))) (string> (aref x (car col)) (aref y (car col))))
					((and (numberp (aref x (car col))) (numberp (aref y (car col)))) (> (aref x (car col)) (aref y (car col))))
					((equal (aref x (car col)) "") t)
					(t nil)))
((and (stringp (aref x (car col))) (stringp (aref y (car col)))) (cond 
																		((string= (aref x (car col)) (aref y (car col))) (desc x y (cdr col)))
																		(t (string> (aref x (car col)) (aref y (car col))))))
((and (numberp (aref x (car col))) (numberp (aref y (car col)))) (cond 
																		((= (aref x (car col)) (aref y (car col))) (desc x y (cdr col)))
																		(t (> (aref x (car col)) (aref y (car col))))))
((equal (aref x (car col)) "") t)
(t nil)
)
)


(defun order-asc(table col)
(sort (get-rows-from-el 1 table) 
#'(lambda (x y)
(cond
((null (cdr col)) (cond 
					((and (stringp (aref x (car col))) (stringp (aref y (car col)))) (string< (aref x (car col)) (aref y (car col))))
					((and (numberp (aref x (car col))) (numberp (aref y (car col)))) (< (aref x (car col)) (aref y (car col))))
					((equal (aref x (car col)) "") nil)
					(t t)))
((and (stringp (aref x (car col))) (stringp (aref y (car col)))) (cond 
																		((string= (aref x (car col)) (aref y (car col))) (asc x y (cdr col)))
																		(t (string< (aref x (car col)) (aref y (car col))))))
((and (numberp (aref x (car col))) (numberp (aref y (car col)))) (cond 
																		((= (aref x (car col)) (aref y (car col))) (asc x y (cdr col)))
																		(t (< (aref x (car col)) (aref y (car col))))))
((equal (aref x (car col)) "") nil)
(t t)
))))
(defun order-desc(table col)
(sort (get-rows-from-el 1 table) 
#'(lambda (x y)
(cond
((null (cdr col)) (cond 
					((and (stringp (aref x (car col))) (stringp (aref y (car col)))) (string> (aref x (car col)) (aref y (car col))))
					((and (numberp (aref x (car col))) (numberp (aref y (car col)))) (> (aref x (car col)) (aref y (car col))))
					((equal (aref x (car col)) "") t)
					(t nil)))
((and (stringp (aref x (car col))) (stringp (aref y (car col)))) (cond 
																		((string= (aref x (car col)) (aref y (car col))) (desc x y (cdr col)))
																		(t (string> (aref x (car col)) (aref y (car col))))))
((and (numberp (aref x (car col))) (numberp (aref y (car col)))) (cond 
																		((= (aref x (car col)) (aref y (car col))) (desc x y (cdr col)))
																		(t (> (aref x (car col)) (aref y (car col))))))
((equal (aref x (car col)) "") t)
(t nil)
))))


(defun create-list-of-collumns(vec lst)
(cond 
((null lst) nil)
((string-intersection (car lst) (vector-to-list vec 0)) (list* (position-col (car lst) vec 0) (create-list-of-collumns vec (cdr lst))))
(t (create-list-of-collumns vec (cdr lst)))
)
)

(defun order(col lst table)
	(let ((colList (create-list-of-collumns  (aref table 0) col)))
		(cond
		((or (null lst) (string-equal (car lst) "asc")) (order-asc table colList))
		((string-equal (car lst) "desc") (order-desc table colList))
		(t table)
	)
)
)

(defun order-by(lst table)
(cond
((null lst) table)
((string-intersection (car (split-by-one-comma (car lst))) (vector-to-list (aref table 0) 0)) (order (split-by-one-comma (car lst)) (cdr lst) table))
(t table)
)
)


;(write (order-by '("col,row" "desc") (load-table "map_zal-skl9.csv") ))