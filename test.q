(run (set! pexp (cons
    (lambda (s c) (apply (lambda (m s c)
        (if (starts-with s m)
            (apply c (gen-var) (drop (length m) s))
            (list)))
        "parse-str" (eval s) (eval c)))
    pexp)))

(run (set! parse-str (lambda (m s c) (apply (lambda (m s c)
    (if (starts-with s m)
        (apply c m (drop (length m) s))
        (list)))
    (eval m) (eval s) (eval c)))))
