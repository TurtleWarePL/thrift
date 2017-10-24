(fiasco:define-test-package (#:setup-tests :in thrift-test:thrift-self-tests)
  (:use #:thrift-test-utils))

(in-package #:setup-tests)

(deftest thrift-class ()
  (let ((class (find-class 'test-struct)))
    (is (equal (thrift:class-identifier class) "TestStruct"))
    (is (every #'(lambda (id name)
                   (equal (thrift:field-definition-identifier
                           (find id (thrift:class-field-definitions class)
                                 :key #'thrift:field-definition-identifier-number))
                          name))
               '(1 2)
               '("fieldOne" "fieldTwo")))))

(deftest test-transport ()
  (is (typep (make-test-transport) 'thrift:binary-transport)))

(deftest test-protocol ()
  (is (typep (make-test-protocol) 'thrift:binary-protocol)))
