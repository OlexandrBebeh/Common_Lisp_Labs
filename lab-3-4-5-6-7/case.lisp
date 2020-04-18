(load "func.lisp")

(defun take-case-ex(lst)
	(cond
		((null lst) nil)
		((string-equal "case" (car lst)) (concatenate 'list (get-list-before-el lst "AS") (list "as") (list (car (cut-list-to-el lst "AS")))))
		(t (take-case-ex (cdr lst)))
	)
)

(defun all-except-case(lst)
	(append 
		(get-except-last (get-list-before-el lst "case"))
		(list (concatenate 'string (get-last (get-list-before-el lst "case")) (car (cut-list-to-el lst "as")) (car (cdr (cut-list-to-el lst "as")))) )
		(cdr (cdr (cut-list-to-el lst "as"))))
)

(defun case-to-end(lst)
	(
		append (all-except-case lst) (take-case-ex lst)
	)
)

(defun check-when-case(val_vec case_val ch)
	(cond
		((char= #\= ch) 
			(if 
				(and (parse-integer case_val :junk-allowed t) (numberp val_vec))
				(= (parse-integer case_val :junk-allowed t) val_vec)
				(if (and (stringp val_vec) (stringp case_val))
					(string= val_vec case_val)
					nil
				)
			)
		)
		((char= #\< ch) 			
			(if 
				(and (numberp (parse-integer case_val :junk-allowed t)) (numberp val_vec))
				(> (parse-integer case_val :junk-allowed t) val_vec)
				(if (and (stringp val_vec) (stringp case_val))
					(string> case_val val_vec)
					nil
				)
			)
		)
		((char= #\> ch) 
				(if 
				(and (parse-integer case_val :junk-allowed t) (numberp val_vec))
				(> (parse-integer case_val :junk-allowed t) val_vec)
				(if (and (stringp val_vec) (stringp case_val))
					(string> case_val val_vec)
					nil
				)
			)
		)
	)
)

(defun parse-when-case(name_cols vec str)
	(cond
		((find #\= str :test #'equalp) (check-when-case 
														(aref vec (position-col (car (split-by-one-equal str)) name_cols 0))
														(car (cdr (split-by-one-equal str)))
														#\=
										)
		)
		((find #\< str :test #'equalp) (check-when-case 
														(aref vec (position-col (car (split-by-one-less str)) name_cols 0))
														(car (cdr (split-by-one-less str)))
														#\<
										)
		)
		((find #\> str :test #'equalp) (check-when-case 
														(aref vec (position-col (car (split-by-one-more str)) name_cols 0))
														(car (cdr (split-by-one-more str)))
														#\>
										)
		)
	)
)

(defun parse-then-val(case_ex)
	(cond
		((null case_ex) nil)
		((string-equal "then" (car case_ex)) (car (cdr case_ex)))
		(t (parse-then-val (cdr case_ex)))
	)
)

(defun make-case-val(name_cols vec case_ex)
	(cond
		((null case_ex) nil)
		((string-equal "when" (car case_ex)) 
			(if 
				(parse-when-case name_cols vec (car (cdr case_ex))) 
				(parse-then-val case_ex) 
				(make-case-val name_cols vec (cdr case_ex))
			)
		)
		((string-equal "else" (car case_ex)) (car (cdr case_ex)))
		(t (make-case-val name_cols vec (cdr case_ex)))
	)
)

(defun build-case-table(table case_ex line)
	(cond
		((= line (length table)) #())
		(t  (concatenate 'vector 
								(vector (concatenate 'vector (aref table line) (vector (make-case-val (aref table 0) (aref table line) case_ex)))) 
								(build-case-table table case_ex (+ line 1))
			)
		)
	)
)

(defun cases(case_ex table)
	(concatenate 'vector
		(vector (concatenate 'vector (aref table 0) (vector (car (cut-list-to-el case_ex "as")))))
		(build-case-table table case_ex 1)
	)
)

(defun start-case(case_ex table)
	(cond
		((null case_ex) table)
		(t (cases case_ex table))
	)
)


;(print-table (select-create-table '("row" "col" "Chtoto" "pos_x") (cases (take-case-ex '("row,col," "case" "when" "row=2" "then" "100" "when" "row>10" "then" "1" "when" "row>20" "then" "0" "else" "10" "end" "as" "Chtoto" ",pos_x" "from" "map_zal" "order" "by" "row")) (load-table "map_zal"))))