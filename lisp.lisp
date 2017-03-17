(new cons (fn (a b) ((fn x x) a . b)))
(new head (fn (x) ((fn (a . b) a) . x)))
(new tail (fn (x) ((fn (a . b) b) . x)))

(new list (fn args args))

(new def (mac (name argspec . body) (list 'new name (list 'fn argspec . body))))
