(run (set! pexp (cons
    (lambda (s c) (apply (lambda (m s c)
        (if (starts-with s m)
            (apply c (gen-var) (list->str (drop (length (str->list m)) (str->list s))))
            (list)))
        "parse-str" (eval s) (eval c)))
    pexp)))
