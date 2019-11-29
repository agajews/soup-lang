(run (set! pexp (cons (lambda (s c) ((eval c) (list) (eval s))) pexp)))

(lambda (s c) (apply (lambda (m s c)
    (if (starts-with s m)
        (c (gen-var) (drop (length m) s))
        (list)))
    "parse-str" s c))
