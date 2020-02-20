(defun sequence(n)
	(cond
	((<= n 0) 1)
	(t (list* (* (expt -1 (+ n 1)) (expt 2 -n)) (sequence (- n 1))))
	))

(write (sequence 6))