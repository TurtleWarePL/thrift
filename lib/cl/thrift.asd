(asdf:defsystem #:thrift
  :depends-on (#:usocket
	       #:trivial-utf-8
	       #:closer-mop)
  :components ((:file "thrift")))
