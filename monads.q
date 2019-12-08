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
    (apply (lambda (var) (lambda (s c)
        (parse-str "declare-var" (eval s) (lambda (_ s)
        (apply (eval c) var (eval s)))))) (gen-var "declare-var"))
    pexp)))

(run (set! define-var (lambda (name val) (apply (lambda (name) (apply (lambda (var) (do
    (set! pexp (cons
        (lambda (s c)
            (parse-str (eval name) (eval s) (lambda (_ s)
            (apply (eval c) var (eval s)))))
        pexp))
    (set! (eval var) val))) (gen-var name))) name))))

(run (define-var "prepend!" (lambda (l x)
    (apply set! l (cons x (eval l))))))

(run (define-var "parse-while" (lambda (p s c) (apply (lambda (l)
    (if (empty (head l))
        (list)
        (apply (eval c) (head l) (head (tail l)))))
    (span (eval p) (eval s))))))

(run (define-var "parse-ws" (lambda (s c)
    (parse-while (lambda (x) (elem (list " " "\n" "\t") x)) (eval s) (eval c)))))

(run (define-var "parse-ident" (lambda (s c)
    (parse-while
        (lambda (x) (or (is-alpha-num x) (elem (str->list "~!@#$%^&*-=+_|'<>?") x)))
        (eval s)
        (lambda (name s) (apply (lambda (c name s)
            (if (all is-digit (split-chars name))
                (list)
                (c name s)))
            c name s))))))

(run (prepend! pexp (lambda (s c)
    (parse-str "(define" (eval s) (lambda (_ s)
    (parse-ws (eval s) (lambda (_ s)
    (parse-ident (eval s) (lambda (name s)
    (parse-ws (eval s) (lambda (_ s)
    (parse (pexp (lambda (val s)
    (parse-str ")" (eval s) (lambda (_ s) (apply
        (lambda (var val) (do
            (prepend! pexp (lambda (s' c')
                (parse-str name (eval s') (lambda (_ s')
                (apply (eval c') (quote var) (eval s'))))))
            (apply (eval c) (quote (apply set! var val)) (eval s))))
        (gen-var)
        (eval val))))))))))))))))))

(define x (+ 3 4))
(define y x)
(* x y)

