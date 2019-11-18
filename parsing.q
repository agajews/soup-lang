(defrule (s)
    (parse int s (x s')
        (parse int s' (y s'')
            (list (list (* x y) s'')))))
