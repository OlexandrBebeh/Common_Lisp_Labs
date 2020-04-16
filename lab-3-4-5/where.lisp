(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)


(load "func.lisp")

(defvar test (simple-table:read-csv #P"test.csv"))


(defun get-rows(lst table)
	(cond 
		((null lst) (simple-table:make-table))
		(t (simple-table:add-to-table (aref table (car lst)) (get-rows (cdr lst) table) ))
	)
)

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


(defun where-equal(lst table)
(
	list-of-equal-rows lst table
))

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

(defun where-less(lst table)
(
	list-of-less-rows lst table)
)

;--------------------------------------------------------------------------------------------------------------------------------------------
(defun where-rows-list-dit-more(dit table n)
(cond
((= (length table) n) nil)
((equal (aref (aref table n) 0) "") (where-rows-list-dit-more dit table (+ n 1)))
((> (aref (aref table n) 0) dit) (list* n (where-rows-list-dit-more dit table (+ n 1))))
(t (where-rows-list-dit-more dit table (+ n 1)))
)
)

(defun where-rows-list-str-more(str table n)
(cond
((= (length table) n) nil)
((string> (aref (aref table n) 0) str) (list* n (where-rows-list-str-more str table (+ n 1))))
(t (where-rows-list-str-more str table (+ n 1)))
)
)

(defun list-of-more-rows(lst table)
(cond
((= (length (car (cdr lst))) (num-dec (parse-integer (car (cdr lst)) :junk-allowed t) 0))
		(where-rows-list-dit-more
		(parse-integer (car (cdr lst)) :junk-allowed t)
		(simple-table:select table (position-col (car lst) (aref table 0) 0)) 1))

(t 		(where-rows-list-str-more
		(car (cdr lst))
		(simple-table:select table (position-col (car lst) (aref table 0) 0)) 1))

)
)

(defun where-more(lst table)
(
	list-of-more-rows lst table)
)

(defun get-list-rows(lst table)
	(cond
		((string-equal (car lst) "not") (set-difference (cdr (reverse (generation (- (length table) 1)))) (get-list-rows (cdr lst) table) ))
		((find #\= (car lst) :test #'equalp) (where-equal (split-by-one-equal (car lst)) table))
		((find #\< (car lst) :test #'equalp) (where-less (split-by-one-less (car lst)) table))
		((find #\> (car lst) :test #'equalp) (where-more (split-by-one-more (car lst)) table))
		(t )
	)
)

(defun conditions(lst table)
(cond
((null lst) nil)
((string-equal (car lst) "not") (list* (get-list-rows lst table) (conditions (cdr (cdr lst)) table)))
((string-equal (car lst) "and") (conditions (cdr lst) table))
((string-equal (car lst) "or") (conditions (cdr lst) table))
(t (list* (get-list-rows lst table) (conditions (cdr lst) table)))
)
)

(defun logic-oper-list(lst)
(cond
((null (cdr lst)) nil)
((string-equal (car lst) "and") (list* (car lst) (logic-oper-list (cdr lst))))
((string-equal (car lst) "or") (list* (car lst) (logic-oper-list (cdr lst))))
(t (logic-oper-list (cdr lst)))
)
)



(defun where-cond(lst operLst)
(cond
((null lst) lst)
((null operLst) lst)
((string-equal (car operLst) "and") (where-cond (list* (intersection (car lst) (car (cdr lst))) (cdr (cdr lst))) (cdr operLst)))
(t (where-cond (list* (union (car lst) (car (cdr lst))) (cdr (cdr lst))) (cdr operLst)))
)
)

(defun list-of-rows-cond(lst table)
	(sort (list* 0 (car (where-cond (conditions lst table) (logic-oper-list lst)))) #'>
	))


(defun where-command(lst table)
(cond 
	((null lst) table)
	((null (cdr lst)) (get-rows (sort (list* 0 (get-list-rows lst table)) #'>) table))
	(t (get-rows (list-of-rows-cond lst table) table))
)
)
;inquiry(SELECT col, pos_x, pos_y From map_zal-skl9.csv where col = 40 or pos_x < 100)
;(write (where-command '("col=2") (load-table "map_zal-skl9.csv")))

;(write (where-cond '((1 2 3) (1 5 6) (2 6 7)) (logic-oper-list '("row=16" "and" "not" "row<10" "or" "row=2"))))