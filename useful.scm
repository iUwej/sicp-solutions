					; replace the first item in a list
;; (define (set-car! list-x item-y)
;;   (cons item-y (cdr list-x))
;;   )

;; ;replace the last item in a pair

;; (define (set-cdr! list-x item-y)
;;   (
;;   )

;return the last pair in  a list
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))
      ))

;create a cycle in a list
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;buggy count -pairs
(define (buggy-count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x)) (count-pairs (cdr x)) 1)))


(define (member? item ylist)
  (cond ((null? ylist)
	 #f
	 )
	((eq? (car ylist) item)
	 #t
	 )
	(else (member? item (cdr ylist))))
  )

(define (count-unique-pairs xlist yset)
  (cond
   ((not (pair? xlist)) 0)
   ((member? xlist yset) 0)
   (else
    (+ (count-unique-pairs (car xlist) (cons xlist yset))
       (count-unique-pairs (cdr xlist)(cons xlist yset))
       1))
   )
  )
