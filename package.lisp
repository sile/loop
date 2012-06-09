(defpackage loop ;; TODO: => flow?
  (:use :common-lisp)
  (:shadow :common-lisp reduce map max min count find)
  (:export make-generator
           def-generator
           from down-from
           for-list 
           for-string
           for-array
           repeat

           make-joint
           map map-n
           filter filter-n
           take take-n
           take-while take-while-n
           drop drop-n
           drop-while drop-while-n
           zip
           
           each
           reduce
           collect
           all? any?
           max min
           find
           count
           sum))
(in-package :loop)

  