;;; Needs the thrift library, shared.lisp and tutorial.lisp loaded

(let ((calc (thrift::binary-client 'calculator-client "127.0.0.1" 9090)))
  (print `(1 + 1 is ,(add calc 1 2))))

(defmethod add ((h handler) num1 num2)
  (format t "~&Asked to add ~A and ~A" num1 num2)
  (+ num1 num2))
