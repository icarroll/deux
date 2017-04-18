(new t 't)
(new nil ())

(mac def (name argspec . body)
     (list 'do
           (list 'new name ())
           (list 'set name (list 'fn argspec . body))))

(def map1 (f xs)
     (if xs (cons (f (head xs)) (map1 f (tail xs)))
       nil))

(def and2 (x y)
     (if x (if y t
             nil)
       nil))
(def or2 (x y)
     (if x t
       (if y t
         nil)))

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

(def alike? (a b)
     (if (same? a b) t
         (and (cons? a) (cons? b))
             (and (alike? (head a) (head b))
                  (alike? (tail a) (tail b)))
          nil))

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
(mac quasiquote (thing)
    (qq thing))

(def these (items)
    (if (null? items) ()
        (cons (head items) (those (tail items)))))

(def those (items)
    (if (null? items) ()
        (these (tail items))))

(mac with (pairs . body)
    `((fn ,(these pairs) ,@body) ,@(those pairs)))

(mac let (var val . body)
    `((fn (,var) ,@body) ,val))
