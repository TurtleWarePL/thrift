;;; Needs the thrift library, shared.lisp and tutorial.lisp loaded

(in-package :thrift-generated)

(defparameter *logs* (make-array 10 :adjustable t :element-type 'integer))

(defmethod getstruct ((h handler) key)
  (aref *logs* key))

(defmethod ping ((h handler))
  (format t "Pinged!~%"))

(defmethod add ((h handler) num1 num2)
  (format t "~&Asked to add ~A and ~A~%" num1 num2)
  (+ num1 num2))

(defmethod calculate ((h handler) logid work)
  (format t "Asked to calculate ~a with logid ~a~%" work logid)
  (let* ((num1 (work-num1 work))
	 (num2 (work-num2 work))
	 (result (case (work-op work)
		   (1 (+ num1 num2))
		   (2 (- num1 num2))
		   (3 (* num1 num2))
		   (4 (if (zerop num2)
			  (error (make-instance 'invalidoperation
						:why "Division by zero"
						:whatop 4))
			  (/ num1 num2)))
		   (t (error (make-instance 'invalidoperation
					    :why "Operation does not exist"
					    :whatop (work-op work)))))))
    (setf (aref *logs* logid) (make-instance 'sharedstruct
					     :key logid
					     :value (format nil "~a" result)))
    result))

(let ((srv (thrift:simple-server "127.0.0.1" 9090)))
  (thrift:serve srv 'calculator-client))
