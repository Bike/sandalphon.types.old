;;;; munge.lisp
;;;; The horror...

(in-package #:sandalphon.types)

(defun munge-deftype-lambda-list (ll &optional (default-default ''*))
  "Given a DEFTYPE lambda list, returns as three values a munged lambda list, the name of a &whole variable or NIL, and the name of an &environment variable or NIL.
\"munged\" here means that &optional and &key defaults have been set up with default-default if they have no specified default; also, any &whole and &environment variable will be stripped (into the values).
This is convenient for DEFTYPEMACRO, which can use the munged list with DESTRUCTURING-BIND, but it's not a very nice operation, hence the ugly name.
The implementation does minimal error checking. The intent is that any errors will become apparent in the DESTRUCTURING-BIND."
  (labels ((rec (ll recursivep)
	     (loop with accum = nil and env = nil and envstate = nil and whole = nil and state = :required
		for (elt . rest) on ll
		do (etypecase elt
		     ((eql &environment) (setf envstate state state '&environment)
		      (when recursivep (error "illegal ~s in nested macro lambda list" '&environment)))
		     ((eql &whole) (setf state '&whole)
		      (when recursivep (push '&whole accum)))
		     ((member &allow-other-keys &aux &body &key &optional &rest) (setf state elt)
		      (push elt accum))
		     ((member #.lambda-list-keywords) (error "~s is illegal in an ~s lambda list" elt 'deftype))
		     (symbol
		      (case state
			((&whole)
			 (when whole (error "multiple ~s variables" '&whole))
			 (setf whole elt state :required)
			 (when recursivep (push elt accum)))
			((&environment)
			 (when env (error "multiple ~s variables" '&environment))
			 (setf env elt state envstate))
			((&optional &key) (push (list elt default-default) accum))
			(t (push elt accum))))
		     (list
		      (case state
			((&whole &environment) (error "lists are not suitable as ~s vars" state))
			((&optional &key)
			 (push (if (null (rest elt)) (list (first elt) default-default) elt) accum))
			((:required) (push (rec elt t) accum))
			(t (push elt accum)))))
		finally (let ((done (nreverse accum)))
			  (when (not (null rest)) (setf (cdr (last done)) rest))
			  (if recursivep (return done) (return (values done whole env)))))))
    (rec ll nil)))

;;; old version, which did a lot more than necessary.

#+jesus-has-returned
(defun munge-deftype-lambda-list (ll &optional (default-default ''*) recursive-p)
  (let (whole-var env-var)
    (labels ((llk-expected (llk)
	       (error "expected lambda-list-keyword after ~s" llk))
	     (multiple (llks)
	       (error "multiple ~{~s~^ or ~} variables" llks))
	     (rebind (name)
	       (error "cannot bind constant variable ~s" name))
	     (assert-previous-state (state ll-elt)
	       ;; FIXME: should be a macro?
	       (assert (find state (rest (member ll-elt '(&aux &allow-other-keys &key &rest &body &optional :required))))
		       ()
		       "Ran into ~s when the previous parsing state was ~s"
		       ll-elt state))
	     (mapcan. (fn check list)
	       ;; handles (x y . z) gracefully
	       (loop with cdr = list with accum-head = (list nil) with accum-tail = accum-head
		  do (etypecase cdr
		       (null (return (cdr accum-head)))
		       (symbol (cond ((constantp list) (rebind list))
				     ((funcall check)
				      (setf (cdr accum-tail) cdr)
				      (return (cdr accum-head)))
				     (t (error ". var in bad state"))))
		       (list (when (setf (cdr accum-tail) (funcall fn (first cdr)))
			       (setf accum-tail (last (cdr accum-tail))))))
		    (setf cdr (cdr cdr))))
	     (monster ()
	       ;; This sucks. It returns two closures.
	       ;; The first one is intended to be mapcan.'d over a lambda list, and does the meat of the parsing.
	       ;; (It returns a munged lambda list.)
	       ;; The second one is just for checking if the parser state is in an adequate state
	       ;;  for a . var pattern.
	       (let (env-previous ; needed since &environment can be fucking anywhere
		     (seen-rest nil) ; for avoiding multiple &rest or &body
		     (state :required))
		 (declare (type ; more for documentation than anything
			   (member &environment &whole &allow-other-keys &aux &body &key &optional &rest :required)
			   state))
		 (values
		  ;; First closure.
		  (lambda (ll-elt)
		    ;; ll-elt is an element of the lambda-list. this is either a keyword, a variable name, or a list.
		    ;; since this is mapcan.'d, we need to return a list of things to be concatenated into the result.
		    ;; this means we can remove an element by returning NIL, too.
		    (etypecase ll-elt
		      ((eql &environment)
		       ;; &environment is illegal below top level
		       (when recursive-p (error "nested ~s" '&environment))
		       ;; save state
		       (setf env-previous state
			     state ll-elt)
		       ;; drop the &environment from the munged ll
		       nil)
		      ((eql &whole)
		       (assert (and (eq state :required) (null whole-var)))
		       (setf state ll-elt)
		       ;; we don't need the &whole in the ll if we're on toplevel.
		       (if recursive-p (list ll-elt) nil))
		      ;; All the other valid keywords are lumped together but still work as states.
		      ((member &allow-other-keys &aux &body &key &optional &rest)
		       (assert-previous-state state ll-elt)
		       (list (setf state ll-elt)))
		      ;; That was all the keywords, so now we're dealing with a variable name.
		      (symbol
		       (when (constantp ll-elt) (rebind ll-elt)) ; can't bind NIL, etc.
		       ;; We have to treat the variable differently depending on the state.
		       (ecase state
			 ((&whole)
			  ;; note we don't need to check for whole-var existing since state is switched back to :required
			  ;; we do that switch because &whole can only be the first variable.
			  (setf whole-var ll-elt
				state :required)
			  ;; if we're munging on the top level we remove the &whole var from the ll
			  ;; (but if we're not we keep it)
			  (if recursive-p (list ll-elt) nil))
			 ;; Basically the whole point of this garbage, in one line. God.
			 ((&optional &key) (list (list ll-elt default-default)))
			 ;; Example of getting here would be (&key a &allow-other-keys b), which is invalid of course.
			 ((&allow-other-keys) (llk-expected '&allow-other-keys))
			 ((&environment)
			  (when env-var
			    (multiple '(&environment)))
			  ;; set the env-var and restore state
			  (setf env-var ll-elt
				state env-previous)
			  ;; drop the variable
			  nil)
			 ((&rest &body)
			  (when seen-rest (multiple '(&rest &body))) ; note that this covers (&rest a b) as well
			  (setf seen-rest state) ; since we do this
			  (list ll-elt))
			 ;; how dull
			 ((:required &aux) (list ll-elt))))
		      ;; OK, our ll-elt is a list.
		      (list
		       (ecase state
			 ;; Recursive pattern! Note that we don't care about the other values.
			 ((:required) (list (munge-deftype-lambda-list ll-elt default-default t)))
			 ((&optional &key)
			  ;; We have to check for the rare but legal (&optional (foo)) case.
			  (list (if (null (cdr ll-elt))
				    ;; replace default
				    (list (first ll-elt) ''*)
				    ;; don't
				    ll-elt)))
			 ((&aux) (list ll-elt))))))
		  ;; Second closure.
		  (lambda ()
		    (find state '(&optional :required)))))))
      ;; And that's it.
      (values (multiple-value-call #'mapcan. (monster) ll) ; (mapcan. parser statechecker ll)
	      whole-var
	      env-var))))

;; less obtuse mapcan.s
#+(or)
(progn
  ;; this one doesn't work for . patterns
  (loop for cdr on list nconcing (funcall fn (first cdr))
     finally (etypecase cdr
	       (null)
	       (symbol (cond ((constantp cdr) (rebind cdr))
			     ((funcall check))
			     (t (error ". var in bad state"))))))
  ;; this one is probably slower if that even matters
  (typecase list
    (null list)
    (symbol (cond ((constantp list) (rebind list))
		  ((funcall check) list)
		  (t (error ". var in bad state"))))
    (list (nconc (funcall fn (first list)) (mapcan. fn check (rest list))))))
