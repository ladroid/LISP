;gnu clisp 2.49

;(print "Hello, world!")

;lab3 task1
(defun fact (n)
  (cond ((equal n 0) 1)
        (t ((lambda (x y)(* x y))
            n (fact (- n 1))
           )
         )
  )
)

(print (fact 5))

(defun fact2 (A) 
	(cond 
		((equal A 0) 1)
		(t (let((C A)
				(D (fact2 (- A 1))))
			(* C D)))
	)
)

(print (fact2 5))

;lab3 task2
(defun deriv (e) 
    (cond ((null e) 0) 
          ((equal e 'x) 1)
          ((atom e) 0)
          ((null (cdr e)) (deriv (car e)))
          ((null (cddr e)); monadic operator +, -, or function id
          (cond ((equal (car e) '+ ) (deriv (cadr e))) ;+
          ((equal (car e) '- ) (list '- (deriv (cadr e))));-
          (t (derfun (car e) (cadr e))) ) ) ; function
          (t (derexpr (car e) (cadr e) (caddr e))) 
    )
)

(defun derexpr (arg1 op arg2 )
    (cond ((equal op '+ ) (deradd arg1 arg2 ))
          ((equal op '- ) (dersub arg1 arg2 ))
          ((equal op '* ) (dermult arg1 arg2))
          ((equal op '/ ) (derdiv arg1 arg2))
          ((equal op '^ ) (derpower arg1 arg2))
          (t (print 'error)) 
    )
)

(defun derfun (fun arg)
    (cond ((equal 'SIN fun) (list (list 'COS arg) '* (deriv arg) ))
          ((equal 'COS fun) (list (list '- (list 'SIN arg)) '*
          (deriv arg) ))
          ((equal 'EXP fun) (list (list 'EXP (list arg)) '*
          (deriv arg) ))
          ((equal 'LOG fun) (list (deriv arg) '/ arg ))
          (t (print 'illegal_function)) 
    )
)

(defun deradd (arg1 arg2)
    (list (deriv arg1) '+ (deriv arg2))
)

(defun dersub (arg1 arg2)
    (list (deriv arg1) '- (deriv arg2))
)

(defun derdiv (arg1 arg2)
    (list (list (list (deriv arg1) '* arg2)
     '- (list arg1 '* (deriv arg2) ))
     '/ (list arg2 '^ '2)
    )
)

(defun dermult (arg1 arg2)
    (list (list (deriv arg1) '* arg2)
     '+ (list arg1 '* (deriv arg2)) 
    )
)

(defun derpower (arg1 arg2)
    (list (list arg1 '^ arg2)
     '* (dermult arg2 (list 'LOG(list arg1)))
    )
)

(print(deriv '(x + 3)))

;lab3 task3 from lab2
(defun q (w n)
    (let ((a w))
         (when (and a (plusp n)) (cons (car a) (q (cdr a) (1- n))))))

(print(q '(1 2) 1))

;lab3 task4
(defun my_eval(A)
	(cond
		((atom A) A)
		((equal 'car    (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)))))
		((equal 'cdr    (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)))))
		((equal 'atom   (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)))))
		((equal 'cons   (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal 'list   (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal 'equal  (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal '*      (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal '/      (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal '+      (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal '-      (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal '=      (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		(T A)
	)
)
(print (my_eval '(+ 1 2)))
(print (my_eval '(+ (car (car (cdr (cdr ((1 2) 3 (4 5)))))) (car (cdr (1 2))))))

;lab3 task5
(defun mrg (w v)
  (merge 'list (sort w #'<) (sort v #'<) #'<))

(defun my_eval(A)
	(cond
        ;((listp A) A)
		((atom A) A)
		((equal 'car    (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)))))
		((equal 'cdr    (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)))))
		((equal 'atom   (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)))))
		((equal 'cons   (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal 'list   (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal 'equal  (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal '*      (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal '/      (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal '+      (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal '-      (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		((equal '=      (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
        ((equal 'mrg    (car A)) (let ((A A)) (funcall (car A) (my_eval (cadr A)) (my_eval (caddr A)))))
		(T A)
	)
)
(print (my_eval '(mrg (list 1 1 2 1 2 1 3) (list 5 3 3 1 2 2))))
;(print (mrg '(1 1 2 1 2 1 3) '(5 3 3 1 2 2)))


;lab3 task6
(defun ss (w)
  (mapcar #'sw (mapcar #'string w)))
 
(defun sw (s)
 (map 'list #'string s))
 
(defun cn (w &optional (n 1) (acc ""))
  (cond ((null w) acc)
        ((equal (car w) (cadr w))
         (cn  (cdr w) (1+ n) acc))
        ((cn (cdr w) 1 (concatenate
                        'string acc
                        (car w)
                        (if (> n 1)
                            (write-to-string n)
                            ""))))))
 
(defun squeeze (w)
  (mapcar #'cn (ss w)))
 
(defun compress-words (w)
  (mapcar #'squeeze w))

(compress-words '((aaabb ccccddd)(eeefggg hhkl)))
(print(compress-words'((aaabb ccccddd)(eeefggg hhkl))))

;lab3 task7
(defun isIn(A B)
	(cond
		((null B) nil)
		((equal A (car B)) T)
		(T (isIn A (cdr B)))
	)
)

(defun isVowel(A)
	(isIn A '(#\а #\е #\ё #\и #\о #\у #\ы #\э #\ю #\я))
)

(defun divideByChar(A)
	(cond 
		((equal A "") nil)
		(T (cons (char A 0) (divideByChar (subseq A 1))))
	)
)

(defun splitSyllables(A B)
	(cond
		((null A) "")
		((equal (car A) #\space) (concatenate 'string " " (splitSyllables (cdr A) T)))
		(B 
			(concatenate 'string 
				(make-string 1 :initial-element (car A)) 
				(splitSyllables (cdr A) (not (isVowel (car A))))
			)
		)
		((isVowel (car A)) 
			(concatenate 'string (make-string 1 :initial-element (car A)) (splitSyllables (cdr A) nil))
		)
		(T 
			(concatenate 'string "_" (make-string 1 :initial-element (car A)) (splitSyllables (cdr A) nil))
		)
	)
)

(defun T7(A)
	(splitSyllables (divideByChar A) T)
)


(print (T7 "Я учу Lisp и его понимаю"))
;(print '("При" "ве" "т" "ми" "р"))

;lab3 task8
(defun deli (word)
    (deli-slovo nil
    (coerce (string word) 'list)))

(defun deli-slovo (begin end)
    (cond
    ((null end) (list begin end))
    ((sogl? (first end))
    (deli-slovo
    (v-end begin (first end))
    (rest end)))
    ((dolgaya-nach? end)
    (list (append begin
    (list (first end)
    (second end)))
    (cddr end)))
    (t (list (v-end begin (first end))
    (rest end)))))

(defun v-end (spisok element)
    (append spisok (list element)))

(defun glasnaya? (letter)
    (member letter *glas*))
    (setq *glas* '(#\A #\E #\I #\O #\U #\Y #\a #\o));
    (defun sogl? (letter)
    (not (glasnaya? letter)))

(defun dolgaya-nach? (word)
    (and (glasnaya? (first word))
    (eql (first word)
    (second word))))

(defun perevedi-slovo(word key)
    (let ((chastislova (deli word))
    (chastikey (deli key)))
    (dolgota-glasnoi (first chastislova)
    (second chastislova)
    (first chastikey)
    (second chastikey))))

(defun dolgota-glasnoi (begin1 end1 begin2 end2)
    (cond
    ((dolgaya-kon? begin1)
    (cond
    ((dolgaya-kon? begin2)
    (pom-chasti begin1 end1 begin2 end2))
    (t (pom-chasti (ukoroti begin1) end1
    (udlinni begin2) end2))))
    ((dolgaya-kon? begin2)
    (pom-chasti
    (udlinni begin1) end2
    (ukoroti begin2) end2))
    (t (pom-chasti begin1 end1 begin2 end2))))

(defun dolgaya-kon? (word)
    (dolgaya-nach? (reverse word)))

(defun ukoroti (slog)
    (if (not (rest slog))
    nil
    (cons (first slog)
    (ukoroti (rest slog)))))

(defun udlinni (slog)
    (if (null (rest slog))
    (cons (first slog) slog)
    (cons (first slog)
    (udlinni (rest slog)))))

(defun pom-chasti
    (begin1 end1 begin2 end2)
    (list (sozv begin1 end1)
    (sozv begin2 end2)))

(defun sozv (begin end)
    (cond (;(perednee begin);
    (soedeni begin (vpered end)))
    (t (soedeni begin (nazad end)))))


(defun vpered (word)
    (sublis
    '((#\U . #\Y) (#\A . #\a) (#\O . #\o))
    word))

(defun nazad (word)
    (sublis
    '((#\Y . #\U) (#\a . #\A) (#\o . #\O))
    word))
    (defun soedeni(begin end)
    (intern (coerce (append begin end)
    'string)))
    
(print (perevedi-slovo 'apple 'banana))

;lab3 task9 
(defparameter *words* '(I am slowly beginning to understand how lisp programming works I like the lisp))
(defparameter *count* nil)

(defun word-count (words)
  (mapcar (lambda (word)
	    (if (eql (assoc word *count*) nil)
		(push (list word 1) *count*)
		(setf (cadr (assoc word *count*)) (1+ (cadr (assoc word *count*))))))
	  words))

(defun print-count (count)
  (mapcar (lambda (entry)
	    (princ (car entry))
	    (princ ": ")
	    (princ (cdr entry))
	    (fresh-line))
	  count))

(word-count *words*)
(print-count *count*)
