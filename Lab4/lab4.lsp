(defun make-ts (production type naming)
(list :production production :type type :naming naming))

;global variable
(defvar *db* nil)

;add in database
(defun add-record (ts) (push ts *db*))

;show datas from db
(defun dump-db ()
(dolist (ts *db*)
(format t "~{~a:~10t~a~%~}~%" ts)))

;select some values
(defun select (selector-fn)
(remove-if-not selector-fn *db*))

;choose a type of selecting
(defun type-selector (type)
(lambda (ts) (equal (getf ts :type) type)))

;generates a selection expression that returns all records of the light-signal armatures that match the values ​​given in where
(defun where (&key production type naming)
(lambda (ts)
(and
(if production (equal (getf ts :production) production) t)
(if type (equal (getf ts :type) type) t)
(if naming (equal (getf ts :naming) naming) t))))

;update db and use key arguments to specify a new value
(defun update (selector-fn &key production type naming (ripped nil ripped-p))
(setf *db*
(mapcar
(lambda (row)
(when (funcall selector-fn row)
(if production (setf (getf row :production) production))
(if type (setf (getf row :type) type))
(if naming (setf (getf row :naming) naming)))
row) *db*)))

;delete some rows from db
(defun delete-rows (selector-fn)
(setf *db* (remove-if selector-fn *db*)))

;find by
(defun make-comparison-expr (field value)
(list 'equal (list 'getf 'ts field) value))

;write to file
(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;read from file
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(add-record (make-ts "АсКо" "СА" "PL1-101"))
(add-record (make-ts "АсКо" "СА" "NXD-211"))
(add-record (make-ts "АсКо" "СА" "AD22C-6"))
(add-record (make-ts "АсКо" "СА" "AD16-16DS"))
(add-record (make-ts "АсКо" "СА" "AD22C-6"))
(add-record (make-ts "АсКо" "СА" "YL238-01"))
(add-record (make-ts "АсКо" "СА" "PL-30N"))

;example
;make select
(print (select (type-selector "СА")))
(format t "~%")

;make select using where and output this row
(print (select (where :naming "PL-30N")))
(format t "~%")

;update
(print (update (where :naming "YL238-01") :naming "YL12-6C"))
(format t "~%")

;finding this row
(print (make-comparison-expr :naming "YL12-6C"))
(format t "~%")

;delete rows
(print (delete-rows (where :naming "YL12-6C")))
(format t "~%")

;output everything
(format t "~%")
(print (dump-db))

;write
(save-db "test.txt")

;read
(format t "~%")
(load-db "test.txt")