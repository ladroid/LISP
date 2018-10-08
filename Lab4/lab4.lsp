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


(make-ts "АсКо" "СА" "PL1-101")
(make-ts "АсКо" "СА" "NXD-211")
(make-ts "АсКо" "СА" "AD22C-6")
(make-ts "АсКо" "СА" "AD16-16DS")
(make-ts "АсКо" "СА" "AD22C-6")
(make-ts "АсКо" "СА" "YL238-01")
(make-ts "АсКо" "СА" "PL-30N")

(add-record (make-ts "АсКо" "СА" "PL1-101"))
(add-record (make-ts "АсКо" "СА" "NXD-211"))
(add-record (make-ts "АсКо" "СА" "AD22C-6"))
(add-record (make-ts "АсКо" "СА" "AD16-16DS"))
(add-record (make-ts "АсКо" "СА" "AD22C-6"))
(add-record (make-ts "АсКо" "СА" "YL238-01"))
(add-record (make-ts "АсКо" "СА" "PL-30N"))

(print (select (type-selector "СА")))

(print (select (where :naming "PL-30N")))

(print (dump-db))