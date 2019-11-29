(run (set! pexp (cons
    (lambda (s c) (apply (lambda (m s c)
        (if (starts-with s m)
            (apply c (gen-var) (drop (length m) s))
            (list)))
        "parse-str" (eval s) (eval c)))
    pexp)))
