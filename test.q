(set! pexp (cons
    (apply (lambda (var m)
        (lambda (s c) (apply (lambda (s c)
            (if (starts-with s m)
                (apply c var (drop (length m) s))
                (list)))
            (eval s) (eval c))))
        (gen-var "parse-str") "parse-str")
    pexp))
