;;; Needs the thrift library, shared.lisp and tutorial.lisp loaded

(in-package :thrift-generated)

(let ((calc (thrift::binary-client 'calculator-client "127.0.0.1" 9090)))
  (print `(1 + 1 is ,(add calc 1 2))))

