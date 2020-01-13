z = (A^T Bx)^T C

args = parse-args
    --verbose/-v (default=True)
    --name/-n (required)
end

net = neuralnet
    batchnorm >=> conv filter(32) layers(5) => pool(max, 2, 2) => relu
    conv filter(16) layers(2) => pool(max, 2, 2) => relu
end


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

do
    parse-str "(define"
    parse-ws
    name <- parse-ident
    val <- parse pexp
    parse-str ")"
