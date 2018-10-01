;gnu clisp 2.49

; —------------------------------------------—

(defun -union (w v)
  (cond ((null w)  v)
    ((-member (car w) v) (-union (cdr w) v))
    ((cons (car w) (-union (cdr w) v)))
  )  
)
 
(defun -member (m w)
  (cond ((atom w) nil)
        ((eql (car w) m) w)
        ((-member m (cdr w)))))

(defun in-predicate (a l)
    (cond
        ((null l) nil)
        ((eq a (car l)) t)
        (t (in-predicate a (cdr l)))
    )
)

(defun inters (a b)
    (cond
        ((null a) nil)
        ((null b) nil)
        ((in-predicate (car a) b) (cons (car a) (inters (cdr a) b)) )
        (t (inters (cdr a) b))
    )
)

(print (inters (-union '(a b) '(c d)) '(b c d)))

; —------------------------------------------—

(print (intersection (union '(a b) '(c d)) '(b c d)))

; —------------------------------------------—

(setq res ())

(defun ch (un a b c)
    (if (or 
            (and (member (car un) a) (member (car un) c))
            (and (member (car un) b) (member (car un) c))
         )
        (push (car un) res)
    )
    (if (atom un) 
        (print "finish")
        (ch (cdr un) a b c)
    )
)

(defun oper (a b c)
    (setq un (union (union a b) c))
    (ch un a b c)
)

(oper '(a b) '(c d) '(b c d))
(print res)