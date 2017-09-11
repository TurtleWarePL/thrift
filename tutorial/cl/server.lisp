;;; Needs the thrift library, shared.lisp and tutorial.lisp loaded

(in-package :thrift-generated)

(defmethod add ((h handler) num1 num2)
  (format t "~&Asked to add ~A and ~A" num1 num2)
  (+ num1 num2))

(let ((srv (thrift:simple-server "127.0.0.1" 9090)))
  (thrift:serve srv 'calculator-client))
