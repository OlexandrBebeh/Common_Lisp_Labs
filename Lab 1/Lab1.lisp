(write ((lambda (list1 list2 list3)
		(list (car list1) 
		      (car list2) 	
		      (car list3)
		      ))'(G55 G66 G777) 
			'(9 (F G) I) 
			'(N I L T D J (II JJ))
	)
)

(terpri)
(terpri)

(defun getatom(i lst)(cond 
			((< i 1) nil)
			((null lst) nil)
			((= i 1) (car lst))
			(t (getatom (- i 1) (cdr lst)))
			))

(defun buildlist(l1 l2 l3)
  (list (getatom 3 l1) (getatom 2 l2) (getatom 6 l3))
)

(write (buildlist '(G55 G66 G777) '(9 (F G) I) '(N I L T D J (II JJ))))
(terpri)
(terpri)
(defun result(A B C U)(
			union A ( 
				union B (
					intersection C (
							set-difference U (
									union A (union 
											(set-difference U B ) (set-difference U C))))))

))

(write (result 	'(G55 G66 G777) 
		'(9 (F G) I) 
		'(N I L T D J (II JJ)) 
		(union '(G55 G66 G777) (	
					union '(9 (F G) I) '(N I L T D J (II JJ))))
))

