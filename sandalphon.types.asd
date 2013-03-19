(asdf:defsystem #:sandalphon.types
  :author "Bike <aeshtaer@gmail.com>"
  :license "WTFPL"
  :version "0.0.1"
  :depends-on (#:alexandria)
  :in-order-to ((test-op (test-op #:sandalphon.types-test)))
  :components ((:file "package")
	       (:file "trivalogic" :depends-on ("package"))
	       (:file "envhack" :depends-on ("package"))
	       (:file "munge" :depends-on ("package"))
	       (:file "parse" :depends-on ("envhack" "munge" "package"))
	       (:file "types" :depends-on ("trivalogic" "package"))
	       (:file "interval" :depends-on ("trivalogic" "package"))
	       (:module "meat"
			;; egh, i don't like the interval dependency, only number needs it
			:depends-on ("types" "package" "interval")
			:components ((:file "named")
				     (:file "tnil" :depends-on ("named"))
				     #+s.t.prim(:file "primitive" :depends-on ("tnil"))
				     (:file "set-types" :depends-on ("tnil"))
				     (:file "satisfies")
				     (:file "member" :depends-on ("tnil"))
				     (:file "number" :depends-on ("named" #+s.t.prim "primitive" "tnil"))
				     (:file "char" :depends-on ("named"))
				     (:file "cons" :depends-on (#+s.t.prim "primitive"))
				     (:file "clos")
				     (:file "array" :depends-on (#+s.t.prim "primitive"))
				     (:file "values")))
	       (:file "specs" :depends-on ("meat" "parse" "package"))
	       (:file "unparse" :depends-on ("meat" "package"))
	       (:file "clos-fixup" :depends-on ("types" "meat" "package"))
	       (:file "disjoin" :depends-on ("types" "meat" "package"))))
