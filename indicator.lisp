(defmacro defdb (name &rest args)
  (let ((glob (symb "*" name "-TABLE*"))
	(add (symb "ADD-" name))
	(make (symb "MAKE-" name))
	(clean (symb "CLEAR-" name "-TABLE"))
	(arg-names (mapcar #'car args)))
    `(progn (defparameter ,glob nil)
	    (defun ,add (,name) 
	      (push ,name ,glob))
	    (defun ,clean () (defparameter ,glob nil))
	    (defun ,make ,arg-names 
	      (list ,@(apply #'append (mapcar #'(lambda (arg) (list (keyw arg) arg)) arg-names))))
	    (defun ,(symb "RANDOM-" name "-TABLE") (n) 
	      (,clean) 
	      (dotimes (i n)
		   (,add (,make ,@(mapcar #'(lambda (arg) (cadr arg)) args))))))))


;; (defun select (table selector-fn)
;;   (remove-if-not selector-fn table))

;; (defun make-comparison-expr (field value)
;;   `(equal (getf arg ,field) ,value))

;; (defun make-comparisons-list (fields)
;;   (loop while fields
;;      collecting (make-comparison-expr (pop fields) (pop fields))))

;; (defmacro where (&rest clauses)
;;   `#'(lambda (arg) (and ,@(make-comparisons-list clauses))))

;; (setq *accident-table* (sort *accident-table* #'< :key #'(lambda (acc) (getf acc :date))))

(defun cluster (series duration)
  (labels ((cluster-helper (series duration acc)
	     (if (null series)
		 (reverse (mapcar #'reverse acc))
		 (let ((head (car series)))
		   (cluster-helper (cdr series)
				   duration
				   (cons (list head)
					 (mapcar #'(lambda (cluster) 
						     (if (< (- (getf head :date) (getf (car (last cluster)) :date)) duration)
							 (cons head cluster)
							 cluster))
						 acc)))))))
    (cluster-helper series duration nil)))



(defun print-cluster (list)
  (format t "~{~{~a   ~}~%~}" list))
;; (defun sinistri (sogg)
;;   (select *accident-table* (where :id sogg)))


;; (print-cluster (cluster (sinistri 7) 5))

(defun restrict (table expression)
  (remove-if-not expression table))


(defun project (table &rest attributes)
  (mapcar #'(lambda (row) (reduce #'(lambda (acc att) 
				      (let ((val (getf row att)))
					(if val
					    (append acc (list att val))
					    acc)))
				  attributes
				  :initial-value nil))
	  table))


(defun product (table1 table2)
  (reduce #'(lambda (acc row1)
	      (append acc (mapcar #'(lambda (row2)
				      (append row1 row2))
				  table2)))
	  table1
	  :initial-value nil))

(defun natjoin (table1 table2)
  (reduce #'(lambda (acc row1)
	      (append acc (remove nil (mapcar #'(lambda (row2)
						  (if (joinable row1 row2)
						      (append row1 (car (apply #'project (list row2) 
									       (set-difference (attributes row2)
											       (common-attributes row1 row2)))))))
					      table2))))
	  table1
	  :initial-value nil))

(defun attributes (row)
  (mapcar #'car (group row 2)))
(defun common-attributes (row1 row2)
  (intersection (mapcar #'car (group row1 2)) (mapcar #'car (group row2 2))))

(defun joinable (row1 row2)
  (reduce #'(lambda (con att)
	      (and con (or (not (getf row2 att)) 
			   (equal (getf row1 att) (getf row2 att)))))
	  row1
	  :initial-value t))


(defparameter row1 '(:a 1 :b 2 :c 3))
(defparameter row2 '(:a 1 :b 2))

;; (pprint (natjoin (list row1) (list row2)))
;; (pprint (natjoin *accident-table* *accident-table*))
;; (pprint (product *accident-table* *accident-table*))

(defun field-values (table field)
  (mapcar #'(lambda (row) (getf row field))
	  table))

(load "d:/giusv/lisp/indy/db.lisp")
