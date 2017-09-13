;;; Needs the thrift library, shared.lisp and tutorial.lisp loaded

(in-package :thrift-generated)

(let ((calc (thrift::binary-client 'calculator-client "127.0.0.1" 9090)))
  (format t "Pinging: ~a~%" (ping calc))
  (format t "1 + 1 = ~a~%" (add calc 1 1))
  (let ((work (make-instance 'work
  			     :op (enum :operation :divide)
  			     :num1 1
  			     :num2 0
  			     :comment "Yo")))
    (handler-case (format t "1 / 0 = ~a~%" (calculate calc 1 work))
      (invalidoperation (io)
	(format t
		"~%Shucks! Invalid operation.~% Why: ~a~% Operation: ~a~%~%"
		(invalidoperation-why io)
		(invalidoperation-whatop io)))))
  (let ((work (make-instance 'work
			     :op (enum :operation :subtract)
			     :num1 15
			     :num2 10
			     :comment "Hi")))
    (format t "15 - 10 = ~a~%" (calculate calc 1 work))))
