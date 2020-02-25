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

(defun build(pos lst)
	(cond 
	((> (+ (* pos 2) 1) (- (list-length lst) 1)) lst)	
	((> (length (nth pos lst)) (length (nth (+ (* pos 2) 1) lst))) (build (+ pos 1) (swapinlist pos (+ (* pos 2) 1) lst)))
	((> (+ (* pos 2) 2) (- (list-length lst) 1)) lst)
	((> (length (nth pos lst)) (length (nth (+ (* pos 2) 2) lst))) (build (+ pos 1) (swapinlist pos (+ (* pos 2) 2) lst)))
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

(write (heapsort '("Lorem" "ipsum" "dolor" "sit" "amet" "consectetur" "adipiscing" "elit" "sed" "do" "eiusmod" "tempor"
"incididunt" "ut" "labore" "et" "dolore" "magna" "aliqua")))

