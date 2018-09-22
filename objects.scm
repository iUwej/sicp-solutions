(define (make-withdraw balance)
	(lambda (amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount)) balance)
			"Insufficient funds"
			)
		)
	)

(define (make-accumulator value)
	(lambda (new-value)
		(set! value (+ value new-value))
		value
		)
	)

(define (make-account balance)
	(define (withdraw amount)
		(if (>= balance amount)
			(begin (set! balance (- balance amount)) balance)
			"Insufficient funds"
			)
		)
	(define (deposit amount)
		(set! balance (+ balance amount))
		balance
		)
	(define (dispatch m)
		(cond ((eq? m 'withdraw) withdraw)
			((eq? m 'deposit) deposit)
			(else (error "Unknown request -- MAKE-ACCOUNT" m)))
		)
	dispatch
	)
;Ex 3.2
 (define (make-monitored f)
 	(define (mf value)
 		(lambda (arg)
 			(cond ((eq? arg 'how-many-calls?) value)
 				((eq? arg 'reset-count)(set! value 0) 0)
 				(else (begin (set! value (+ value 1)) (f arg)))
 				)
 			)
 	)
 	(mf 0))


					; Ex 3.3

	(define (make-protected-account balance secret-password)

	  (define (withdraw amount)
	    (if (>= balance amount)
		(begin (set! balance (- balance amount)) balance)
		"Insufficient fund")
	    )
	  (define (deposit amount)
	    (set! balance(+ balance amount))
	    balance
	    )
	  (define (dispatch password m)
	    (if (eq? secret-password password)
		(cond
		 ((eq? m 'withdraw) withdraw)
		 ((eq? m 'deposit) deposit)
		 (else (error "Uknown request -- MAKE-PROTECTED-ACCOUNT" m)))
		(error "Incorrect-password"))
	    )
	  dispatch)

					; Ex 3.4


(define (make-vaulted-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount)) balance)
	"Insufficient fund")
    )
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance
    )

  (define (call-the-cops args) "Calling the cops, on you")

  (define (make-vaulted count )

    (lambda (password m)
      (if (eq? secret-password password)
	  (begin (set! count 0)
		 (cond
		  ((eq? m 'withdraw) withdraw)
		  ((eq? m 'deposit) deposit)
		  (else (error "Uknown request -- MAKE-PROTECTED-ACCOUNT" m))))
	  (begin (set! count (+ 1 count))  (if(>= count 7) call-the-cops  (error "Incorrect-password")))
	  ) )
    )
  (make-vaulted 0))


					; Ex 3.5

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials)
	   )
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)

  (define  (experiment)
    (P (random-in-range x1  x2) (random-in-range y1 y2)))

  (monte-carlo trials experiment))


	   
  
