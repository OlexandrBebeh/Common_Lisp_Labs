(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(load "join.lisp")

(defun filter-group(val col_val table col line)
	(cond
		((= line (length table)) '())
		((and (stringp (aref (aref table line) col)) (string= "" (aref (aref table line) col))) (filter-group val col_val table col (+ 1 line)))
		((and (stringp val) (stringp (aref (aref table line) col_val)))
																		(cond
																		((string= val (aref (aref table line) col_val)) (append (list (aref (aref table line) col)) (filter-group val col_val table col (+ 1 line)) ))
																		(t (filter-group val col_val table col (+ 1 line)))
																		)
		)
		((and (numberp val) (numberp (aref (aref table line) col_val))) 
																		(cond
																		((= val (aref (aref table line) col_val)) (append (list (aref (aref table line) col)) (filter-group val col_val table col (+ 1 line)) ))
																		(t (filter-group val col_val table col (+ 1 line)))
																		)
		)
		(t (filter-group val col_val table col (+ 1 line)))
	)
)

(defun avg-group(val col_val col table)
(let ((lst (filter-group val col_val table col 1)))
	(if (or (numberp lst) (null lst))
		lst
		(floor (reduce #'+ lst) (length lst)))
))

(defun count-group(val col_val col table)
(let ((lst (filter-group val col_val table col 1)))
	(if (or (numberp lst) (stringp lst))
	1
	(length lst)
)
))

(defun max-group(val col_val col table)
(let ((lst (filter-group val col_val table col 1)))
	(if (or (numberp lst) (stringp lst))
	lst
	(maximum lst)
)
))


(defun which-fun(val col_val str table)
	(cond
		((search "Avg(" str :test #'char-equal) (avg-group val col_val (position-col (cut-parameter str) (aref table 0) 0) table))
		((search "count(" str :test #'char-equal) (count-group val col_val (position-col (cut-parameter str) (aref table 0) 0) table))
		((search "max(" str :test #'char-equal) (max-group val col_val (position-col (cut-parameter str) (aref table 0) 0) table))
		(t (write "not func"))
	)
)

(defun build-row(val col_val lst table)
(cond
	((null lst) #())
	((chekc-function (car lst)) (concatenate 'vector (vector (which-fun val col_val (car lst) table)) (build-row val col_val (cdr lst) table)))
	(t (concatenate 'vector (vector val) (build-row val col_val (cdr lst) table)))
)
)

(defun build-groups(dist lst table col_val line)
	(cond
		((= line (length dist)) #())
		(t (concatenate 'vector (vector (build-row (aref (aref dist line) 0) col_val lst table)) (build-groups dist lst table col_val (+ line 1))))
	)
)

(defun group-by(col_val lst table)
	(
		build-groups (simple-table:distinct table col_val) lst table col_val 1
	)
)


(defun parser-group-by(cols lst table)
	(
		concatenate 'vector 
							(vector (list-to-vec cols))
							(group-by (position-col (car lst) (aref table 0) 0) cols table)
	)
)

(defun having-filter(val table col line ch)
	(cond
		((= line (length table)) #())
		((char= #\= ch) (cond
							((and (stringp val) (stringp (aref (aref table line) col))) (cond
																							((string= val (aref (aref table line) col)) (concatenate 'vector (vector (aref table line)) (having-filter val table col (+ line 1) ch)))
																							(t (having-filter val table col (+ line 1) ch))
																							))
							((and (numberp val) (numberp (aref (aref table line) col)))(cond
																							((= val (aref (aref table line) col)) (concatenate 'vector (vector (aref table line)) (having-filter val table col (+ line 1) ch)))
																							(t (having-filter val table col (+ line 1) ch))
																							))
							(t (having-filter val table col (+ line 1) ch))
						))
		((char= #\< ch) (cond
							((and (stringp val) (stringp (aref (aref table line) col))) (cond
																							((string> val (aref (aref table line) col)) (concatenate 'vector (vector (aref table line)) (having-filter val table col (+ line 1) ch)))
																							(t (having-filter val table col (+ line 1) ch))
																							))
							((and (numberp val) (numberp (aref (aref table line) col)))(cond
																							((> val (aref (aref table line) col)) (concatenate 'vector (vector (aref table line)) (having-filter val table col (+ line 1) ch)))
																							(t (having-filter val table col (+ line 1) ch))
																							))
							(t (having-filter val table col (+ line 1) ch))
						))
		((char= #\> ch) (cond
							((and (stringp val) (stringp (aref (aref table line) col))) (cond
																							((string< val (aref (aref table line) col)) (concatenate 'vector (vector (aref table line)) (having-filter val table col (+ line 1) ch)))
																							(t (having-filter val table col (+ line 1) ch))
																							))
							((and (numberp val) (numberp (aref (aref table line) col)))(cond
																							((< val (aref (aref table line) col)) (concatenate 'vector (vector (aref table line)) (having-filter val table col (+ line 1) ch)))
																							(t (having-filter val table col (+ line 1) ch))
																							))
							(t (having-filter val table col (+ line 1) ch))
						))
	)
)

(defun having(lst table ch)
	(let ((val (parse-integer (car (cdr lst)) :junk-allowed t))
		(col (position-col (car lst) (aref table 0) 0)))
	(cond
		((char= #\= ch) (if 
			val
			 (having-filter val table col 1 #\=) 
			 (having-filter (car (cdr lst)) table col 1 #\=)))
		((char= #\< ch) (if 
			val
			 (having-filter val table col 1 #\<) 
			 (having-filter (car (cdr lst)) table col 1 #\<)))
		((char= #\> ch) (if 
			val
			 (having-filter val table col 1 #\>) 
			 (having-filter (car (cdr lst)) table col 1 #\>)))
	)
	)
)

(defun check-having(str table)
	(cond
		((find #\= str :test #'equalp) (having (split-by-one-equal str) table #\=))
		((find #\< str :test #'equalp) (having (split-by-one-less str) table #\<))
		((find #\> str :test #'equalp) (having (split-by-one-more str) table #\>))
	)
)

(defun start-having(lst table)
(cond
	((null lst) table)
	((equal (car lst) nil) table)
	(t (concatenate 'vector (vector (aref table 0)) (check-having (car lst) table)))
)
)

(defun having-group-by(cols lst table)
	(cond
		((string-equal "by" (car lst)) (order-by (cdr (cut-list-to-el lst "order")) (start-having (split-by-one-comma (car (cut-list-to-el lst "having"))) (parser-group-by cols (cdr lst) table))))
		(t (start-group-by cols (cdr lst) table ))
	)
)

(defun start-group-by(lst)
	(cond
		((null lst) nil)
		((and (string-intersection "group" lst) (string-intersection "join" lst)) (having-group-by
																								 (split-by-one-comma (car lst))
																								  (cut-list-to-el lst "group")
																								   (select-join (concatenate 'list '("*") (get-list-before-el (cdr lst) "group") ))))
		((string-intersection "group" lst) (having-group-by
															(split-by-one-comma (car lst))
															  (cut-list-to-el lst "group")
																   (select-inquiry (concatenate 'list '("*") (get-list-before-el (cdr lst) "group")))))
		(t (write "ooops"))
	)
)
;(print-table (having-group-by '("row" "count(col)" "max(pos_x)" "Avg(pos_y)") '("by" "row" "having" "count(col)<10" "order" "by" "max(pos_x)" "desc") (load-table "map_zal")))

