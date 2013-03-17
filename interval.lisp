(in-package #:sandalphon.types)

(defclass interval ()
  ((low :accessor interval-low :initarg :low)
   (high :accessor interval-high :initarg :high)
   (openness :accessor interval-openness :initarg :openness
	     :type (member :left :both :right :neither))))

(defmethod print-object ((object interval) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (display-interval object stream)))

(defun interval-specifier (interval)
  (with-slots (low high openness) interval
    (list (if (or (eq low '*) (member openness '(:right :neither)))
	      low
	      (list low))
	  (if (or (eq high '*) (member openness '(:left :neither)))
	      high
	      (list high)))))

(defun display-interval (interval &optional stream)
  (with-slots (low high openness) interval
    (let ((left-open (member openness '(:left :both)))
	  (right-open (member openness '(:right :both))))
      (when left-open (write-char #\( stream))
      (write low :stream stream)
      (when left-open (write-char #\) stream))
      (write-char #\Space stream)
      (when right-open (write-char #\( stream))
      (write high :stream stream)
      (when right-open (write-char #\) stream))))
  interval)

(defun make-interval (low high)
  (make-instance 'interval :low (ensure-car low) :high (ensure-car high)
		 :openness (merge-openness (listp low) (listp high))))

(defun interval-union/2 (i1 i2)
  (when (lb< i2 i1) (rotatef i1 i2))
  (if (disjoint-p i1 i2)
      nil
      (conjoin-intervals i1 i2)))

(defun interval-intersection/2 (i1 i2)
  (when (lb< i2 i1) (rotatef i2 i1))
  (disjoin-intervals i1 i2))

(defun lb< (i1 i2)
  (let ((i1l (interval-low i1)) (i2l (interval-low i2))
	(i1o (interval-openness i1)) (i2o (interval-openness i2)))
    (cond ((eq i1l '*) (not (eq i2l '*)))
	  ((eq i2l '*) nil)
	  ((or (member i1o '(:left :both)) (member i2o '(:right :neither))) (< i1l i2l)) ; (0 < (0 or [0 < [0 or (0 < [0
	  (t (<= i1l i2l))))) ; [0 < (0

(defun disjoint-p (i1 i2)
  (let ((i1h (interval-high i1)) (i2l (interval-low i2))
	(i1o (interval-openness i1)) (i2o (interval-openness i2)))
    (cond ((or (eq i2l '*) (eq i1h '*)) nil)
	  ((or (member i1o '(:left :neither)) (member i2o '(:right :neither))) (not (<= i2l i1h)))
	  (t (not (< i2l i1h)))))) ; 0) < (0

(defun merge-openness (left-open-p right-open-p)
  (cond ((and left-open-p right-open-p) :both)
	(left-open-p :left)
	(right-open-p :right)
	(t :neither)))

(defun conjoin-intervals (i1 i2)
  (let ((i1l (interval-low i1)) (i2l (interval-low i2))
	(i1h (interval-high i1)) (i2h (interval-high i2))
	(i1o (interval-openness i1)) (i2o (interval-openness i2)))
    (multiple-value-bind (new-low left-open-p)
	(cond ((or (eq i1l '*) (eq i2l '*)) (values '* t))
	      ((< i1l i2l) (values i1l (member i1o '(:left :both))))
	      ((= i1l i2l) (values i1l (and (member i1o '(:left :both)) (member i2o '(:left :both)))))
	      (t (values i2l (member i2o '(:left :both)))))
      (multiple-value-bind (new-high right-open-p)
	  (cond ((or (eq i1h '*) (eq i2h '*)) (values '* t))
		((> i1h i2h) (values i1h (member i1o '(:right :both))))
		((= i1h i2h) (values i1h (and (member i1o '(:right :both)) (member i2o '(:right :both)))))
		(t (values i2h (member i2o '(:right :both)))))
	(make-instance 'interval :low new-low :high new-high
		       :openness (merge-openness left-open-p right-open-p))))))

(defun disjoin-intervals (i1 i2)
  (let ((i1l (interval-low i1)) (i2l (interval-low i2))
	(i1h (interval-high i1)) (i2h (interval-high i2))
	(i1o (interval-openness i1)) (i2o (interval-openness i2)))
    (multiple-value-bind (new-low left-open-p)
	(cond ((eq i1l '*) (values i2l (member i2o '(:left :both))))
	      ((eq i2l '*) (values i1l (member i1o '(:left :both))))
	      ((< i1l i2l) (values i2l (member i2o '(:left :both))))
	      ((= i1l i2l) (values i1l (or (member i1o '(:left :both)) (member i2o '(:left :both)))))
	      (t (values i2l (member i2o '(:left :both)))))
      (multiple-value-bind (new-high right-open-p)
	(cond ((eq i1h '*) (values i2h (member i2o '(:right :both))))
	      ((eq i2h '*) (values i1h (member i1o '(:right :both))))
	      ((> i1h i2h) (values i2h (member i2o '(:right :both))))
	      ((= i1h i2h) (values i1h (or (member i1o '(:right :both)) (member i2o '(:right :both)))))
	      (t (values i1h (member i2o '(:right :both)))))
	(make-instance 'interval :low new-low :high new-high
		       :openness (merge-openness left-open-p right-open-p))))))

(defun subintervalp (i1 i2)
  (let ((i1l (interval-low i1)) (i2l (interval-low i2))
	(i1h (interval-high i1)) (i2h (interval-high i2))
	(i1o (interval-openness i1)) (i2o (interval-openness i2)))
    (and (cond ((eq i1l '*) (eq i2l '*))
	       ((eq i2l '*) t)
	       ((< i1l i2l) nil)
	       ((= i1l i2l) (or (member i1o '(:left :both)) (member i2o '(:right :neither))))
	       (t t))
	 (cond ((eq i1h '*) (eq i2h '*))
	       ((eq i2h '*) t)
	       ((> i1h i2h) nil)
	       ((= i1h i2h) (or (member i1o '(:right :both)) (member i2o '(:left :neither))))
	       (t t)))))

(defun in-interval-p (object interval)
  (let ((il (interval-low interval)) (ih (interval-high interval)) (io (interval-openness interval)))
    (and (cond ((eq il '*) t)
	       ((> object il) t)
	       ((= object il) (member io '(:right :neither)))
	       (t nil))
	 (cond ((eq ih '*) t)
	       ((< object ih) t)
	       ((= object ih) (member io '(:left :neither)))
	       (t nil))
	 t)))

#||

(defclass interval ()
  ((low :accessor interval-low :initarg :low)
   (high :accessor interval-high :initarg :high)))

(defun make-interval (low high) (make-instance 'interval :low low :high high))

(defgeneric interval-union/2 (i1 i2)
  (:documentation
   "Given two intervals, return a new interval of their union, or NIL if this is impossible
\(e.g. there's a gap between them)."))

(defmethod interval-union/2 ((i1 interval) (i2 interval))
  (with-slots ((i1l low) (i1h high)) i1
    (with-slots ((i2l low) (i2h high)) i2
      (let ((new-low
	     (cond ((bound< i1l i2l)
		    ;; disjoint intervals check
		    ;; we use unless >= instead of when < so that we treat surely not and unknown the same
		    (unless (bound>= i1h i2l)
		      (return-from interval-union/2 nil))
		    i1l)
		   ((bound= i1l i2l) i1l)
		   ((bound< i2l i1l)
		    (unless (bound>= i2h i1l)
		      (return-from interval-union/2 nil))
		    i2l)
		   ;; unknown either way
		   (t (return-from interval-union/2 nil))))
	    (new-high
	     (cond ((bound> i1h i2h)
		    (unless (bound<= i1l i2h)
		      (return-from interval-union/2 nil))
		    i1h)
		   ((bound= i1h i2h) i1h)
		   ((bound> i2h i1h)
		    (unless (bound<= i2l i1h)
		     (return-from interval-union/2 nil))
		    i1h)
		  (t (return-from interval-union/2 nil)))))
	(make-interval new-low new-high)))))

(defgeneric interval-intersection/2 (i1 i2)
  (:documentation
   "Given two intervals, return a new interval of their intersection or NIL if this would be empty."))

(defmethod interval-intersection/2 ((i1 interval) (i2 interval))
  (with-slots ((i1l low) (i1h high)) i1
    (with-slots ((i2l low) (i2h high)) i2
      (let ((new-low (if (bound< i1l i2l) i2l i1l))
	    (new-high (if (bound> i1h i2h) i2h i1h)))
	(if (bound> new-low new-high)
	    nil
	    (make-interval new-low new-high))))))

(defgeneric bound< (b1 b2)
  (:documentation "Trivalent < on bounds.")
  (:method (b1 b2) (values nil nil))
  (:method ((b1 real) (b2 real)) (values (<= b1 b2) t))
  (:method ((b1 real) (b2 cons)) (values (if (car b2) (< b1 (car b2)) (<= b1 (cdr b2))) t))
  (:method ((b1 real) (b2 (eql '-*))) (values nil t))
  (:method ((b1 real) (b2 (eql '*))) (values t t))
  (:method ((b1 cons) (b2 real)) (values (if (cdr b1) (<= (cdr b1) b2) (< (car b1) b2)) t))
  (:method ((b1 cons) (b2 cons)) (values (< (or (car b1) (cdr b1)) (or (car b2) (cdr b2))) t))
  (:method ((b1 cons) (b2 (eql '-*))) (values nil t))
  (:method ((b1 cons) (b2 (eql '*))) (values t t))
  (:method ((b1 (eql '-*)) (b2 real)) (values t t))
  (:method ((b1 (eql '-*)) (b2 cons)) (values t t))
  (:method ((b1 (eql '-*)) (b2 (eql '-*))) (values nil t))
  (:method ((b1 (eql '-*)) (b2 (eql '*))) (values t t))
  (:method ((b1 (eql '*)) (b2 real)) (values nil t))
  (:method ((b1 (eql '*)) (b2 cons)) (values nil t))
  (:method ((b1 (eql '*)) (b2 (eql '-*))) (values nil t))
  (:method ((b1 (eql '*)) (b2 (eql '*))) (values nil t)))

(defgeneric bound= (b1 b2)
  (:documentation "Trivalent = on bounds.")
  (:method (b1 b2) (values nil nil))
  (:method ((b1 real) (b2 real)) #|| ha, ha, i'm uncomputable ||# (values (= b1 b2) t))
  (:method ((b1 real) (b2 cons)) (values nil t))
  (:method ((b1 real) (b2 (eql '-*))) (values nil t))
  (:method ((b1 real) (b2 (eql '*))) (values nil t))
  (:method ((b1 cons) (b2 real)) (values nil t))
  (:method ((b1 cons) (b2 cons)) (values (= (or (car b1) (cdr b1)) (or (car b2) (cdr b2))) t))
  (:method ((b1 cons) (b2 (eql '-*))) (values nil t))
  (:method ((b1 cons) (b2 (eql '*))) (values nil t))
  (:method ((b1 (eql '-*)) (b2 real)) (values nil t))
  (:method ((b1 (eql '-*)) (b2 cons)) (values nil t))
  (:method ((b1 (eql '-*)) (b2 (eql '-*))) (values t t))
  (:method ((b1 (eql '-*)) (b2 (eql '*))) (values nil t))
  (:method ((b1 (eql '*)) (b2 real)) (values nil t))
  (:method ((b1 (eql '*)) (b2 cons)) (values nil t))
  (:method ((b1 (eql '*)) (b2 (eql '-*))) (values nil t))
  (:method ((b1 (eql '*)) (b2 (eql '*))) (values t t)))

(defgeneric bound<= (b1 b2)
  (:documentation "Trivalent <= on bounds.")
  (:method (b1 b2) (tri/or (bound< b1 b2) (bound= b1 b2))))

(defgeneric bound> (b1 b2)
  (:documentation "Trivalent > on bounds.")
  ;; intuitionism? screw that
  (:method (b1 b2) (tri/not (bound<= b1 b2))))

(defgeneric bound>= (b1 b2)
  (:documentation "Trivalent >= on bounds.")
  (:method (b1 b2) (tri/not (bound< b1 b2))))

(defun in-interval-p (object interval)
  (tri/if (bound>= object (interval-low interval))
	  (tri/if (bound<= object (interval-high interval))
		  t
		  nil
		  (error "bad interval"))
	  nil
	  (error "bad interval")))

(defun subintervalp (i1 i2)
  ;; generic?
  "Is i1 contained in i2? Trivalent."
  (tri/and
   (bound>= (interval-low i1) (interval-low i2))
   (bound<= (interval-high i1) (interval-high i2))))

(define-compiler-macro subintervalp (&whole whole i1 i2)
  (if (and (consp i2)
	   #+magic(eq #'make-interval (fdefinition (first i2) env)) ; shadowing!
	   (eq 'make-interval (first i2))) ; :(
      (once-only (i1)
	`(tri/and
	  (bound>= (interval-low ,i1) ,(second i2))
	  (bound<= (interval-high ,i1) ,(third i2))))
      whole))

||#
