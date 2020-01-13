(run (set! pexp (cons
    (apply (lambda (var m)
        (lambda (s c) (apply (lambda (s c)
            (if (starts-with s m)
                (apply c var (drop (length m) s))
                (list)))
            (eval s) (eval c))))
        (gen-var "parse-str") "parse-str")
    pexp)))

(run (set! parse-str (lambda (m s c) (apply (lambda (m s c)
    (if (starts-with s m)
        (apply c m (drop (length m) s))
        (list)))
    (eval m) (eval s) (eval c)))))

(run (set! pexp (cons
    (apply
        (lambda (var) (lambda (s2 c)
            (parse-str "declare-var" (eval s2) (lambda (_ s3)
            (apply (eval c) var (eval s3))))))
        (gen-var "declare-var"))
    pexp)))

(set! define-var (lambda (name val) (apply (lambda (name) (apply (lambda (var) (do
    (set! pexp (cons
        (lambda (s0 c)
            (parse-str (eval name) (eval s0) (lambda (_ s1)
            (apply (eval c) var (eval s1)))))
        pexp))
    (set! (eval var) val))) (gen-var name))) name)))
