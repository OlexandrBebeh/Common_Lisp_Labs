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
	((> (+ (* pos 2) 1) (- (list-length lst) 1)) lst)	
	((< (nth pos lst) (nth (+ (* pos 2) 1) lst)) (build (+ pos 1) (swapinlist pos (+ (* pos 2) 1) lst)))
	((> (+ (* pos 2) 2) (- (list-length lst) 1)) lst)
	((< (nth pos lst) (nth (+ (* pos 2) 2) lst)) (build (+ pos 1) (swapinlist pos (+ (* pos 2) 2) lst)))
	(t (build (+ pos 1) lst))
	))

(defun heapbuild(n lst)
	(cond
	((< n 1) lst)
	(t (heapbuild (- n 1) (build 0 lst)) )
	))

(defun carheap(lst)
	(cond
	((null lst) nil)
	(t (list* (car lst) (carheap (build 0 (cdr lst))) ))
))

(defun heapsort(lst)
	(carheap (heapbuild (+ (/ (list-length lst) 2) 1) lst))
)

(write (heapsort (seq 7)))

