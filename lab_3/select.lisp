(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)


(defvar test (simple-table:read-csv #P"test.csv"))

(defun split-by-one-dot (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\. string :start i)
          collect (subseq string i j)
          while j)
)

(defun get-last(lst)
	(cond 
		((null (cdr lst)) (car lst))
		(t (get-last (cdr lst)))
		)
)
(defun generation(n)
	(cond
		((= n 0) nil)
		(t (list* (- n 1) (generation (- n 1))))
	)
)

(defun load-table(tableName)
	(cond
		((string= (get-last (split-by-one-dot tableName)) "csv") (simple-table:read-csv tableName t))
		((string= (get-last (split-by-one-dot tableName)) "tsv") (simple-table:read-tsv tableName t))
		(t (print "file type not define"))
)
)

(defun split-by-one-comma(string)
    (loop for i = 0 then (1+ j)
          as j = (position #\, string :start i)
          collect (subseq string i j)
          while j)
)

(defun vector-to-list(vec n)
	(cond
		((= (length vec) n) nil)
		(t (list* (aref vec n) (vector-to-list vec (+ n 1))))
	)
)


(defun string-intersection(str lst)
	(cond
		((null lst) nil)
		((string-equal str (car lst)) t)
		(t (string-intersection str (cdr lst)))
	)
)

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
			(t (print (simple-table:select1 table listOfCol)))
		))
)


(defun chek-from(cols lst)
(cond
((string-equal "FROM" (car lst)) (select-create-table cols (simple-table:read-csv (car (cdr lst)) t)))
(t (pprint "FROM not found"))
)
)

(defun select-inquiry(lst)
(cond
((string-equal (car lst) "distinct") nil)
(t (chek-from (split-by-one-comma (car lst)) (cdr lst)))
)
)
(defun foo1(lst1  lst2)
(cond
	((null lst1) nil)
	(t (list ))
)
)
;(write (simple-table:read-csv (car '("test.csv"))))
;(write (get-list-of-collumns (simple-table:get-row 0 test) '("20" "30")))

;(write (string-intersection "20" (vector-to-list (simple-table:get-row 0 test) 0)))

;(write (coerce (aref (simple-table:get-row 0 test) 6) 'character))
