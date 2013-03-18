(asdf:defsystem :sandalphon.types-test
  :depends-on (#:sandalphon.types #:alexandria #:fiveam)
  :version "0.1"
  :components ((:module "test"
			:components ((:file "package")
				     (:file "util" :depends-on ("package"))
				     (:file "figures" :depends-on ("package"))
				     (:file "basic" :depends-on ("util" "package"))
				     (:file "standard-atomic-subtypery" :depends-on ("basic" "util" "package"))
				     (:file "class" :depends-on ("basic" "figures" "package"))
				     (:file "parse" :depends-on ("basic" "figures" "package"))
				     (:file "set" :depends-on ("basic" "util" "package")))))
  ;; cargo-culted from bordeaux-threads-test
  :in-order-to ((asdf:test-op (asdf:load-op sandalphon.types-test))))

(defmethod asdf:perform ((operation asdf:test-op) (component (eql (find-system :sandalphon.types-test))))
  (funcall (find-symbol "RUN!" :fiveam)
	   (find-symbol "ALL" :sandalphon.types-test)))
