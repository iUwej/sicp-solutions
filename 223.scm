(define (scale-tree tree factor)
	(cond ((null? tree) nil)
		((not (pair? tree))(* tree factor))
		(else (cons (scale-tree (car tree) factor)
			(scale-tree (cdr tree) factor)))
		)
	)

;method to filter items given a predicate
(define (filter predicate sequence)
	(cond ((null? sequence) nil)
		((predicate (car sequence))
			(cons (car sequence ) (filter predicate (cdr sequence))))
		(else (filter predicate (cdr sequence)))
		))

;implementation of accumulation procedure
(define (accumulate op initial sequence)
	(if (null? sequence) initial
	(op (car sequence)
		(accumulate op initial (cdr sequence)))))

; generate a list of integers in a given range
(define (enumerate-interval low high)
	(if (> low high) 
		nil
		(cons low (enumerate-interval (+ low 1) high))

		)

	)

;enumerate the leaves of a tree
(define (enumerate-tree tree)
	(cond ((null? tree) nil)
		((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
			(enumerate-tree (cdr tree))))))

;exercise 2.33
(define (map p sequence)
	(accumulate (lambda (x y) (cons (p x) y)) nil sequence))

;append procedure using accumulator
(define (append seq1 seq2)
	(accumulate cons seq2 seq1)
	)

;length of  a sequence using the accumulator method
(define (length sequence)
	(accumulate (lambda (x y) (+ 1 y))  0 sequence))

