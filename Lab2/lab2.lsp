;lab2 task1
;delete all i+n-e elements
(defun wx (w n)
  (when (and w (plusp n)) (cons (car w) (wx (cdr w) (1- n)))))
(print (wx '(1 2) 1))

;lab2 task2
;1. making insertion func
;2. func with optional parameters
;3. func shell sort with gaps
;4. func sedgwick gaps
(defun insertion (lst x)
   (cond ((null lst) (LIST x))
         ((> (car lst) x) (cons x lst))
         (t (cons (car lst) (insertion (cdr lst) x)))))

(defun isort (x &optional (s nil))
   (cond ((null x) s)
         (t (isort (cdr x) (insertion s (car x))))))

(defun shell (lst gap finalLIST)
 (cond ((null lst) finalLIST)
 (T (cond ((> gap (LENGTH lst)) (APPEND finalLIST (isort lst)))

  (T (APPEND finalLIST (isort (subseq lst 0 gap)) (shell (subseq lst gap (LENGTH lst)) gap finalLIST)))))))

(defun shellSort (lst gaps) 
 (cond ((null (cdr gaps)) (shell lst (car gaps) '()))
  (T (shellSort (shell lst (car gaps) '()) (cdr gaps)))))

(defun SedgewickGaps (len finalLIST) 
 '(1 8 23 77 281 1073 4193 16577 65921 262913 1050113 4197377 16783361 
   67121153 268460033 1073790977 4295065601 17180065793 68719869953 274878693377 
   1099513200641 4398049656833 17592192335873 70368756760577))

(defun sortWithShellAndSedgewick (lst) 
 (shellSort lst (SedgewickGaps (LENGTH lst) '(1))))

(print (sortWithShellAndSedgewick '(7 6 5 4 3 1 2 0)))

;lab2 task3
;two functions
;which breaks the list 
;relative to the reference element
;and then make func quick
(defun qsort (L)
  (cond
    ((null L) nil)
    (t
      (append
        (qsort (list< (car L) (cdr L)))
        (cons (car L) nil) 
        (qsort (list>= (car L) (cdr L)))))))

(defun list< (a b)
  (cond
    ((or (null a) (null b)) nil)
    ((< a (car b)) (list< a (cdr b)))
    (t (cons (car b) (list< a (cdr b))))))

(defun list>= (a b)
  (cond
    ((or (null a) (null b)) nil)
    ((>= a (car b)) (list>= a (cdr b)))
    (t (cons (car b) (list>= a (cdr b))))))
(print(qsort '(10 4 2 5 9 1 3 7 6 8)))

;lab2 task4
;merge tail of lists
(DEFUN merge_LISTs 
    (LIST1 LIST2)
    (COND ((NULL LIST1) LIST2)
          ((NULL LIST2) LIST1)
          ((> (CAR LIST1) (CAR LIST2)) (CONS (CAR LIST2) (merge_LISTs LIST1 (CDR LIST2))))
          (T (CONS (CAR LIST1) (merge_LISTs (CDR LIST1) LIST2)))))
 
(print (merge_LISTs '(1 2 3) '(1 2 3 4 5)))

;lab2 task5
;max elem in list
(defun DEPTH (X)
    (cond((atom X) 0)
    (T((lambda (e1 e2)
    (if (> e1 e2) e1 e2))
    (1+ (DEPTH (car X)))(DEPTH (cdr X))))))
(print (DEPTH'((1 6) 7 (((8) 4) 3))))