;(new quote (special quote))
;(new if (special if))
;(new fn (special fn))
;(new mac (special mac))
;(new new (special new))
;(new set (special set))
;(new builtin (special builtin))

(new t 't)
(new nil ())

(new cons (fn (a b) ((fn x x) a . b)))
(new head (fn (x) ((fn (a . b) a) . x)))
(new tail (fn (x) ((fn (a . b) b) . x)))

(new list (fn args args))

(new def (mac (name argspec . body) (list 'new name (list 'fn argspec . body))))
(new defmac (mac (name argspec . body) (list 'new name (list 'mac argspec . body))))

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

(def reduce-ss (f xs x v)
     (if (alike? x v) v
         (null? xs) x
         (reduce-ss f (tail xs) (f x (head xs)) v)))

(def and xs
     (reduce-ss and2 xs t nil))

(def or xs
     (reduce-ss or2 xs nil t))

(def reduce (f xs x)
     (if (null? xs) x
         (reduce f (tail xs) (f x (head xs)))))

(def append2 (xs ys)
     (if xs (if ys (cons (head xs)
                         (append2 (tail xs) ys))
              xs)
       ys))

(def append args
     (if (null? args) ()
       (reduce append2 (tail args) (head args))))

(new sym? (builtin issym))
(new cons? (builtin iscons))

(new same? (builtin issame))
(def alike? (a b)
     (if (same? a b) t
         (and (cons? a) (cons? b))
             (and (alike? (head a) (head b))
                  (alike? (tail a) (tail b)))
          nil))

(new = same?)
(new + (builtin plus))
(new - (builtin minus))
(new * (builtin times))

(def len (xs)
     (if (null? xs) 0
       (+ 1 (len (tail xs)))))

(def map (f xs)
     (if (null? xs) ()
         (cons (f (head xs)) (map f (tail xs)))))

(def second (xs)
     (head (tail xs)))

(def qq (x)
     (if (cons? x)
             (if (same? 'unquote (head x)) (second x)
                 (same? 'quasiquote (head x)) (qq (qq (second x)))
                 (cons? (head x))
                     (if (same? 'unquote-splicing (head (head x)))
                             (list 'append (second (head x)) (qq (tail x)))
                         (list 'cons (qq (head x)) (qq (tail x))))
                 (list 'cons (qq (head x)) (qq (tail x))))
         (list 'quote x)))
(defmac quasiquote (thing)
    (qq thing))

(def these (items)
    (if (null? items) ()
        (cons (head items) (those (tail items)))))

(def those (items)
    (if (null? items) ()
        (these (tail items))))

(defmac let (pairs . body)
    `((fn ,(these pairs) ,@body) ,@(those pairs)))

(defmac with (var val . body)
    `((fn (,var) ,@body) ,val))
