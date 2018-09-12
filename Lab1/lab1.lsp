;task1
(setq LIST_1 '(GOAL FUNCTOR CLAUSE (DATA BASE)))
(setq LIST_2 '(2 5(5 4 6) 8))
(setq LIST_3 '(L (K (K I) U)))

(print (LIST (CAR LIST_1) (CAR LIST_2) (CAR LIST_3)))

;task2
(DEFUN CONCAT_LISTS
       (L1 L2 L3) 
       (list (NTH 4 L1) (NTH 3 L2) (NTH 2 L3))
)
(print (CONCAT_LISTS LIST_1 LIST_2 LIST_3))

;task3
(defun task (lst)
  (let ((s1 (nth 0 lst))
        (s3 (nth 2 lst))
        (s7 (nth 6 lst)))
    (if (and (numberp s1) (numberp s3) (numberp s7)) (+ s1 s3 s7) (car (last lst)))))

(setq tasking '(1 2 3 4 5 6 7 8))
(print(task tasking))
