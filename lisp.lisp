(new t 't)
(new nil ())

(new cons (fn (a b) ((fn x x) a . b)))
(new head (fn (x) ((fn (a . b) a) . x)))
(new tail (fn (x) ((fn (a . b) b) . x)))

(new list (fn args args))

(new def (mac (name argspec . body) (list 'new name (list 'fn argspec . body))))

(def map (f xs)
     (if xs (cons (f (head xs)) (map f (tail xs)))
       nil))

(def not (c)
     (if c nil t))
(def and2 (x y)
     (if x (if y t
             nil)
       nil))
(def or2 (x y)
     (if x t
       (if y t
         nil)))

(new null? not)

(def append2 (xs ys)
     (if xs (if ys (cons (head xs)
                         (append2 (tail xs) ys))
              xs)
       ys))

(new cons? (builtin iscons))
(new same? (builtin issame))

(def alike? (a b)
     (if (same? a b) t
       (and2 (cons? a) (cons? b))
           (and2 (alike? (head a) (head b))
                 (alike? (tail a) (tail b)))
        nil))

(new = same?)
(new + (builtin plus))
(new - (builtin minus))
(new * (builtin times))

(def len (xs)
     (if (null? xs) 0
       (+ 1 (len (tail xs)))))
