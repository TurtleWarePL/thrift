(in-package #:common-lisp-user)

(defpackage #:thrift-test
  (:use #:common-lisp)
  (:export #:thrift-self-tests))

(defpackage #:thrift-test-utils
  (:shadowing-import-from #:thrift #:byte #:set #:list #:map #:type-of #:float)
  (:use #:thrift
        #:cl)
  (:import-from #:trivial-gray-streams
                #:stream-write-byte
                #:stream-read-byte
                #:stream-read-sequence
                #:stream-write-sequence
                #:stream-force-output
                #:stream-finish-output)
  (:export #:field-three
           #:make-test-large-struct
           #:make-test-protocol
           #:make-test-struct
           #:make-test-transport
           #:reset
           #:rewind
           #:stream-write-byte
           #:stream-read-byte
           #:stream-read-sequence
           #:stream-write-sequence
           #:stream-force-output
           #:stream-finish-output
           #:test-large-struct
           #:test-large-struct-field-one
           #:test-large-struct-field-two
           #:test-large-struct-field-three
           #:test-struct
           #:test-struct-field-one
           #:test-struct-field-two
           #:with-test-services))

(defpackage #:thrift-test-request (:use))

(defpackage #:thrift-test-response (:use))
