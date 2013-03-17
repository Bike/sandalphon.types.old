(asdf:defsystem :sandalphon.types-test
  :depends-on (#:sandalphon.types #:alexandria #:fiveam)
  :version "0.1"
  :components ((:module "test"
			:components ((:file "package")
				     (:file "figures" :depends-on ("package"))
				     (:file "basic" :depends-on ("figures" "package")))))
  ;; cargo-culted from bordeaux-threads-test
  :in-order-to ((asdf:test-op (asdf:load-op sandalphon.types-test))))

(defmethod asdf:perform ((operation asdf:test-op) (component (eql (find-system :sandalphon.types-test))))
  (funcall (find-symbol "RUN!" :fiveam) :sandalphon.types))
