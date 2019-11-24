(run (set! pexp (cons
    (lambda (s c) (apply (lambda (m s c)
        (if (starts-with s m)
            (c (gen-var) (drop (length m) s))
            (list))) "parse-str" s c))
    pexp)))

(run (set! parse-str (lambda (m s c) (apply (lambda (m s c)
    (if (starts-with s m)
        (c m (drop (length m) s))
        (list))) m s c))))

(run (set! pexp (cons
    (lambda (s c)
        (parse-str "prepend!" (eval s) (lambda (_ s)
        ((eval c) (gen-var) (eval s)))))
    pexp)))

(run (set! prepend! (lambda (l x) (set! l (cons (eval x) (eval l))))))

(run (set! pexp (cons
    (lambda (s c)
        (parse-str "parse-while" (eval s) (lambda (_ s)
        ((eval c) (gen-var) (eval s)))))
    pexp)))

(run (prepend! pexp (lambda (s c)
    (parse-str "(define" (eval s) (lambda (_ s)
    (parse-ws (eval s) (lambda (_ s)
    (parse-ident (eval s) (lambda (name s)
    (parse-ws (eval s) (lambda (_ s)
    (parse (pexp (lambda (v s)
    (parse-str ")" (eval s) (lambda (_ s) ((lambda (var) (do
        (prepend! pexp (lambda (s' c')
            (parse-str name (eval s') (lambda (_ s')
            ((eval c') (eval var))))))
        (set! (eval var) (eval v))
        ((eval c) (list) (eval s)))) (gen-var))))))))))))))))))


