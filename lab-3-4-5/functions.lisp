(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(load "func.lisp")



(defun max-str(lst str)
	(cond 
		((null lst) str)
		((string< str (car lst)) (max-str (cdr lst) (car lst)))
		(t (max-str (cdr lst) str))
	)
)

(defun maximum (lst)
  (cond
  	((stringp (car lst)) (max-str lst ""))
  	(t (reduce #'max lst))))

(defun Max-func(col table)
(
simple-table:add-to-row 
(maximum (get-col-list (position-col col (aref table 0) 0 ) table 1)) 
(simple-table:add-to-row (concatenate 'string "Max " col) (simple-table:make-row))
)
)

(defun Avg(lst)
(
	floor (reduce #'+ lst) (length lst)
)
)

(defun Avg-func(col table)
(
simple-table:add-to-row 
(Avg (get-col-list (position-col col (aref table 0) 0 ) table 1)) 
(simple-table:add-to-row (concatenate 'string "Avg " col) (simple-table:make-row))
)
)

(defun Count-func(col table)
(
simple-table:add-to-row 
(length (get-col-list (position-col col (aref table 0) 0 ) table 1)) 
(simple-table:add-to-row (concatenate 'string "Count " col) (simple-table:make-row))
)
)

(defun db-func(lst table)
(cond
((null lst) (simple-table:make-table))
((search "Avg(" (car lst) :test #'char-equal) (simple-table:add-to-table (Avg-func (cut-parameter (car lst)) table) (db-func (cdr lst) table)))
((search "Max(" (car lst) :test #'char-equal) (simple-table:add-to-table (Max-func (cut-parameter (car lst)) table) (db-func (cdr lst) table)))
((search "Count(" (car lst) :test #'char-equal) (simple-table:add-to-table (Count-func (cut-parameter (car lst)) table) (db-func (cdr lst) table)))
(t table)
)
)
;(princ (db-func '("Count(id_mp)" "Avg(col)" "Max(pos_y)") (load-table "map_zal-skl9.csv")))