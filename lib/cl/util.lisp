(in-package :org.apache.thrift.implementation)

(defun split-once (delimiter sequence)
  "Find the first instance of DELIMITER and split the sequence into two."
  (let ((position (position delimiter sequence)))
    (if (null position) (error "The expected delimiter was not found."))
    (values (subseq sequence 0 position)
	    (subseq sequence (1+ position)))))
