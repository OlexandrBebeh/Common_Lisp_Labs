(defun seq(i)
	(cond
	((<= i 0) nil)
	(t (list* (* (expt -1 (+ i 1)) (expt 2 (* -1 i))) (seq (- i 1))))
	))

(write (seq 8))