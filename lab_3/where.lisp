(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)


(defvar test (simple-table:read-csv #P"test.csv"))


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
((string= str (car lst)) (list* (car lst) (string-intersection str (cdr lst))))
(t (list* nil (string-intersection str (cdr lst))))
)
)

(defun select-create-table(cols table)
	(string-intersection (simple-table:get-row 0 table) cols)
)

(defun select-execute(cols table)
	(pprint ()
	)
)

(defun chek-from(cols lst)
(cond
((string-equal "FROM" (car lst)) (select-execute cols (cdr lst)))
(t (pprint "FROM not found"))
)
)

(defun select-inquiry(lst)
(cond
((string-equal (car lst) "distinct") nil)
(t (chek-from (split-by-one-comma (car lst)) (cdr lst))))
)
  
;(write (string-intersection "20" (vector-to-list (simple-table:get-row 0 test))))

(write (coerce (aref (simple-table:get-row 0 test) 6) 'character))
