(run (set! pexp (cons (lambda (s c) (apply (eval c) (list) (eval s))) pexp)))
