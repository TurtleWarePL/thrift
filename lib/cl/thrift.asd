(asdf:defsystem #:thrift
  :depends-on (#:alexandria
               #:usocket
	       #:trivial-utf-8
	       #:closer-mop)
  :components ((:file "thrift")))
