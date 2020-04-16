(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defvar map_zal (simple-table:read-csv "map_zal-skl9.csv" t))
(defvar mp_assistants (simple-table:read-csv "mp-assistants.csv" t))
(defvar plenary_register_mps (simple-table:read-tsv "plenary_register_mps-skl9.tsv" t))

(defun chekc-function(str)
(cond
((search "Avg(" str :test #'char-equal) t)
((search "Max(" str :test #'char-equal) t)
((search "Count(" str :test #'char-equal) t)
(t nil)
)
)

(defun list-to-vec(lst)
	(cond
		((null lst) #())
		(t (concatenate 'vector (vector (car lst)) (list-to-vec (cdr lst))))
	)
)

(defun cut-parameter (command)
	  (subseq command (+ (position #\( command) 1) (position #\) command :from-end t))
  )

(defun filter-str(lst str)
	(cond
		((null lst) nil)
		((string-equal str (car lst)) (filter-str (cdr lst) str))
		(t (list* (car lst) (filter-str (cdr lst) str)))
	)
)

(defun split-by-one-dot (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\. string :start i)
          collect (subseq string i j)
          while j))

(defun split-by-one-equal (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\= string :start i)
          collect (subseq string i j)
          while j))

(defun split-by-one-more (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\> string :start i)
          collect (subseq string i j)
          while j))

(defun split-by-one-less (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\< string :start i)
          collect (subseq string i j)
          while j))

(defun get-last(lst)
	(cond 
		((null (cdr lst)) (car lst))
		(t (get-last (cdr lst)))
		)
)



(defun load-table(tableName)
	(cond
		((string= tableName "map_zal")  map_zal)
		((string= tableName "mp_assistants") mp_assistants)
		((string= tableName "plenary_register_mps") plenary_register_mps)
		((string= (get-last (split-by-one-dot tableName)) "csv") (simple-table:read-csv tableName t))
		((string= (get-last (split-by-one-dot tableName)) "tsv") (simple-table:read-tsv tableName t))
		(t nil)
)
)

(defun print-spaces(n)
	(cond
		((< n 0) )
		(t  (princ " ")
			(print-spaces (- n 1))
		)
	)
)

(defun print-vec(vec n lenVec)
	(cond 
		((= n (length vec)) )
		((stringp (aref vec n)) (cond 
									((string= "" (aref vec n)) 
																(princ " | ")
																(print-spaces (- (aref lenVec n) 3))
																(write NIL)
																(print-vec vec (+ n 1) lenVec)
									)
									((char= #\Return (char (aref vec n) 0)) 
																			(princ " | ")
																			(print-spaces (- (aref lenVec n) 3))
																			(write NIL)
																			(print-vec vec (+ n 1) lenVec)
									)
									((char= #\Return (char (aref vec n) (- (length (aref vec n)) 1))) 
																										(princ " | ")
																										(print-spaces (- (aref lenVec n) (length (aref vec n))))
																										(write (remove #\Return (aref vec n)))
																										(print-vec vec (+ n 1) lenVec)
									)
									(t  (princ " | ")
										(print-spaces (- (aref lenVec n) (length (aref vec n))))
										(princ (aref vec n))
										(print-vec vec (+ n 1) lenVec)
										)
									))
		((numberp (aref vec n))
										(princ " | ")
										(print-spaces (- (aref lenVec n) (length (write-to-string (aref vec n)))))
										(princ (aref vec n))
										(print-vec vec (+ n 1) lenVec)
		)
		(t  ;(princ(format nil (concatenate 'string "|" (write-to-string (aref lenVec n)) "A") (aref vec n)))
			(princ " | ")
			(print-spaces (- (aref lenVec n) (length (aref vec n))))
			(princ (aref vec n))
			(print-vec vec (+ n 1) lenVec)
			)
	)
)



(defun print-table-full(tab n lenVec)
(cond
((= n (length tab)) )
(t 	(terpri)
	(print-vec (aref tab n) 0 lenVec)
	(print-table-full tab (+ n 1) lenVec))
)
)

(defun max-length(tab col line m)
	(cond
		((= line (length tab)) m)
		((stringp (aref (aref tab line) col))	(cond 
												((< m (length (aref (aref tab line) col)) ) (max-length tab col (+ 1 line) (length (aref (aref tab line) col))))
												(t (max-length tab col (+ 1 line) m))
											)
		)
		((numberp (aref (aref tab line) col))   (cond
												((< m (length (write-to-string (aref (aref tab line) col))) ) (max-length tab col (+ 1 line) (length (write-to-string (aref (aref tab line) col)))))
												(t (max-length tab col (+ 1 line) m))
											)
		)
		(t (max-length tab col (+ 1 line) m))
	)
)

(defun vec-of-max-len(tab col)
	(cond
		((= col (length (aref tab 0))) #())
		(t (concatenate 'vector  (vector (max-length tab col 0 0)) (vec-of-max-len tab (+ col 1) )))
	)
)


(defun print-table(tab)
	(print-table-full tab 0 (vec-of-max-len tab 0) )
)
;(princ (length "Шевченко Євгеній Володимирович"))

;(print-table map_zal)
(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))


(defun split-by-one-comma(string)
    (loop for i = 0 then (1+ j)
          as j = (position #\, string :start i)
          collect (subseq string i j)
          while j))


(defun read-file(nameFile)
(let ((in (open nameFile :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
         while line do (format t "~a~%" line))
    (close in)))
)


(defun generation(n)
	(cond
		((= n 0) nil)
		(t (list* (- n 1) (generation (- n 1))))
	)
)

(defun vector-to-list(vec n)
	(cond
		((= (length vec) n) nil)
		(t (list* (aref vec n) (vector-to-list vec (+ n 1))))
	)
)
(defun string-intersection(str lst)
	(cond
		((equal str nil) nil)
		((null lst) nil)
		((string-equal str (car lst)) t)
		(t (string-intersection str (cdr lst)))
	)
)

(defun cut-list-to-el(lst str)
(cond
((null lst) lst)
((string-equal (car lst) str) (cdr lst))
(t (cut-list-to-el (cdr lst) str))
)
)

(defun position-col(str vec n)
(cond
((string-equal str (aref vec n)) n)
(t (position-col str vec (+ n 1)))
)
)

(defun check-symb(str)
(and (alpha-char-p (char str 0)) (alpha-char-p (char str (- (length str) 1)))
)
)


(defun concat(str1 str2)
(cond
((equal str2 nil) str1)
((equal str1 nil) str2)
((and (check-symb str1) (check-symb str2)) (concatenate 'string str1 " " str2))
(t (concatenate 'string str1 str2))
)
)

(defun get-list-before-el(lst el)
(cond
((null lst) nil)
((string-equal (car lst) el) nil)
(t (list* (car lst) (get-list-before-el (cdr lst) el)))
))


(defun get-col-list(col table n)
(cond
	((= n (length table)) nil)
	((equal "" (aref (aref table n) col)) (get-col-list col table (+ n 1)))
	(t (list* (aref (aref table n) col) (get-col-list col table (+ n 1))))
))


(defun make-collumn-names(vec str n)
	(cond
	((= n (length vec)) #())
	(t (concatenate 'vector (vector (concatenate 'string str "." (aref vec n))) (make-collumn-names vec str (+ n 1))))
	)
)

(defun split-by-last-dot(str)
	(list
		(subseq str 0 (position #\. str :from-end t))
		(subseq str (+ (position #\. str :from-end t) 1) (length str))
	) 
)


(defun get-names-table(lst)
	(cond
		((null lst) NIL)
		(t (concatenate 'list (split-by-last-dot (car lst)) (get-names-table (cdr lst))))
	)
)

(defun make-empty-vec(n)
	(cond
		((= n 0) #()) 
		(t (concatenate 'vector (vector "") (make-empty-vec (- n 1))))
	)
)

(defun concat-to-table-vectors(vec lst)
	(cond
		((null lst) #())
		((null (cdr lst)) (vector (concatenate 'vector vec (car lst))))
		(t (concatenate 'vector (vector (concatenate 'vector vec (car lst))) (concat-to-table-vectors vec (cdr lst))))
	)
)


(defun concat-to-table-vectors-right(vec lst l)
	(cond
		((null lst) (vector (concatenate 'vector (make-empty-vec l) vec)))
		((null (cdr lst)) (vector (concatenate 'vector (car lst) vec)))
		(t (concatenate 'vector (vector (concatenate 'vector (car lst) vec)) (concat-to-table-vectors vec (cdr lst))))
	)
)

(defun concat-to-table-vectors-left(vec lst l)
	(cond
		((null lst) (vector (concatenate 'vector  vec (make-empty-vec l))))
		((null (cdr lst)) (vector (concatenate 'vector vec (car lst))))
		(t (concatenate 'vector (vector (concatenate 'vector (car lst) vec)) (concat-to-table-vectors vec (cdr lst))))
	)
)
