(load "scheme/stdlib.scm")

(define-syntax (bind-vars bindings)
    `(map car bindings))

(define-syntax (bind-vals bindings)
    `(map cadr bindings))

(define-syntax (let bindings body)
    `(apply (lambda ,(bind-vars bindings) ,body) ',(bind-vals bindings)))


(define-syntax (while condition body)
  `(let ((loop ()))
     ('if (,condition)
	    (begin ,body)
	    (loop)))) 

(define-syntax (nil! var) `(set! ,var '()))

(define-syntax (switch var body)
    `(fold (lambda (accum pair) ('if ,(equal? (car pair) var) (cadr pair) '())) 
        '()
        ,body)) 
 