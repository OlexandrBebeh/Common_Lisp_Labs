(require 'asdf)
(load "cl-simple-table-master/cl-simple-table.asd")
(asdf:load-system 'cl-simple-table)

(defun split-by-one-dot (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\. string :start i)
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
		((string= (get-last (split-by-one-dot tableName)) "csv") (simple-table:read-csv tableName t))
		((string= (get-last (split-by-one-dot tableName)) "tsv") (simple-table:read-tsv tableName t))
		(t (print "file type not define"))
)
)

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
		((null lst) nil)
		((string-equal str (car lst)) t)
		(t (string-intersection str (cdr lst)))
	)
)

