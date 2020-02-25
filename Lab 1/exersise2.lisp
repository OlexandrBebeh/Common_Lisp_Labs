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

(write (swapinlist (nth 2 '(0 1 2 3 4 5 6)) (nth 3 '(0 1 2 3 4 5 6)) '(0 1 2 3 4 5 6)))

