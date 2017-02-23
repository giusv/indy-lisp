(defmacro defdb (name &rest args)
  (let ((glob (symb "*" name "-TABLE*"))
	(add (symb "ADD-" name))
	(make (symb "MAKE-" name))
	(clean (symb "CLEAR-" name "-TABLE")))
    `(progn (defparameter ,glob nil)
	    (defun ,add (,name) 
	      (push ,name ,glob))
	    (defun ,clean () (defparameter ,glob nil))
	    (defun ,make ,args 
	      (list ,@(apply #'append (mapcar #'(lambda (arg) (list (keyw arg) arg)) args))))
	    (defun ,(symb "RANDOM-" name "-TABLE") (n) 
	      (,clean) 
	      (dotimes (i n)
		   (,add (,make ,@(mapcar #'(lambda (*) `(random 10)) args))))))))


(defun select (table selector-fn)
  (remove-if-not selector-fn table))

(defun make-comparison-expr (field value)
  `(equal (getf arg ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (arg) (and ,@(make-comparisons-list clauses))))

(defdb accident id date)
(random-accident-table 100)
(sort *accident-table* #'< :key #'(lambda (acc) (getf acc :date)))
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
(defun sinistri (sogg)
  (select *accident-table* (where :id sogg)))


(print-cluster (cluster (sinistri 7) 5))

