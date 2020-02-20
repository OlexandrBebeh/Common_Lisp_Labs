(defun setelem(el n lst)
	(cond
	((< n 0) nil)
	((null lst) nil)
	((= n 0) (list* el (cdr lst)))
	(t (list* (car lst) (setelem el (- n 1) (cdr lst)))
)))

(defun swap(el1 el2 pos1 pos2 lst)
	(setelem el1 pos2 (setelem el2 pos1 lst)) )

(defun swapinlist(pos1 pos2 lst)
	(setelem (nth pos1 lst) pos2 (setelem (nth pos2 lst) pos1 lst)))

(defun seq(i)
	(cond
	((<= i 0) nil)
	(t (list* (* (expt -1 (+ i 1)) (expt 2 (* -1 i))) (seq (- i 1))))
))

(defun build(pos lst)
	(cond 
	((> (+ (* pos 2) 1) (list-length lst)) )
	((< (nth pos lst) (nth (+ (* pos 2) 1) lst))  (swapinlist pos (+ (* pos 2) 1) lst))
	((< (nth pos lst) (nth (+ (* pos 2) 2) lst))  (swapinlist pos (+ (* pos 2) 2) lst))
	(t (build (+ pos 1) lst))
	))

(write (build 0 '(7 1213 12890 728 8107 929 192 8091)))