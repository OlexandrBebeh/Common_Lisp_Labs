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
		(union '(G55 G66 G777) (union '(9 (F G) I) '(N I L T D J (II JJ))))
))

