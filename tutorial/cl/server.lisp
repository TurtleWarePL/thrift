;;; Needs the thrift library, shared.lisp and tutorial.lisp loaded

(let ((srv (thrift:simple-server "127.0.0.1" 9090)))
  (thrift:serve srv 'calculator-client))
