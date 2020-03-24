(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)


(load "func.lisp")

(defvar test (simple-table:read-csv #P"test.csv"))

(defun num-dec(digit n)
(cond
((equal digit nil) -1)
((>= digit 1) (num-dec (/ digit 10) (+ n 1)))
(t n)
)
)

(defun where-rows-list-dit(dit table n)
(cond
((= (length table) n) nil)
((equal (aref (aref table n) 0) "") (where-rows-list-dit dit table (+ n 1)))
((= (aref (aref table n) 0) dit) (list* n (where-rows-list-dit dit table (+ n 1))))
(t (where-rows-list-dit dit table (+ n 1)))
)
)

(defun where-rows-list-str(str table n)
(cond
((= (length table) n) nil)
((string= (aref (aref table n) 0) str) (list* n (where-rows-list-str str table (+ n 1))))
(t (where-rows-list-str str table (+ n 1)))
)
)

(defun list-of-equal-rows(lst table)
(cond
((= (length (car (cdr lst))) (num-dec (parse-integer (car (cdr lst)) :junk-allowed t) 0))
		(where-rows-list-dit
		(parse-integer (car (cdr lst)) :junk-allowed t)
		(simple-table:select table (position-col (car lst) (aref table 0) 0)) 1))

(t 		(where-rows-list-str
		(car (cdr lst))
		(simple-table:select table (position-col (car lst) (aref table 0) 0)) 1))

)
)
(defun get-rows(lst table)
	(cond 
		((null lst) (simple-table:make-table))
		(t (simple-table:add-to-table (aref table (car lst)) (get-rows (cdr lst) table) ))
	)
)

(defun where-equal(lst table)
(get-rows
	(append (list-of-equal-rows lst table) '(0))
	table
)
)
;---------------------------------------------------------------------------------------------------
(defun where-rows-list-dit-less(dit table n)
(cond
((= (length table) n) nil)
((equal (aref (aref table n) 0) "") (where-rows-list-dit-less dit table (+ n 1)))
((< (aref (aref table n) 0) dit) (list* n (where-rows-list-dit-less dit table (+ n 1))))
(t (where-rows-list-dit-less dit table (+ n 1)))
)
)

(defun where-rows-list-str-less(str table n)
(cond
((= (length table) n) nil)
((string< (aref (aref table n) 0) str) (list* n (where-rows-list-str-less str table (+ n 1))))
(t (where-rows-list-str-less str table (+ n 1)))
)
)

(defun list-of-less-rows(lst table)
(cond
((= (length (car (cdr lst))) (num-dec (parse-integer (car (cdr lst)) :junk-allowed t) 0))
		(where-rows-list-dit-less
		(parse-integer (car (cdr lst)) :junk-allowed t)
		(simple-table:select table (position-col (car lst) (aref table 0) 0)) 1))

(t 		(where-rows-list-str-less
		(car (cdr lst))
		(simple-table:select table (position-col (car lst) (aref table 0) 0)) 1))

)
)
(defun get-rows-less(lst table)
	(cond 
		((null lst) (simple-table:make-table))
		(t (simple-table:add-to-table (aref table (car lst)) (get-rows (cdr lst) table) ))
	)
)

(defun where-less(lst table)
(get-rows-less
	(append (reverse (list-of-less-rows lst table)) '(0))
	table
)
)

(defun where-command(lst table)
(cond
((null lst) table)
((find #\= (car lst) :test #'equalp) (where-equal (split-by-one-equal (car lst)) table))
((find #\< (car lst) :test #'equalp) (where-less (split-by-one-less (car lst)) table))
(t (write "don`t undestand operation after where"))
)
)

;(write (where-less (split-by-one-less "row<10") (load-table "map_zal-skl9.csv")))

