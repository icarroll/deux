(new cons (fn (a b) ((fn x x) a . b)))
(new head (fn (x) ((fn (a . b) a) . x)))
(new tail (fn (x) ((fn (a . b) b) . x)))

(new list (fn args args))
