(defpackage loop
  (:use :common-lisp)
  (:shadow :common-lisp reduce map max min count)
  (:export collect
           each
           reduce
           all?
           any?
           max
           min
           sum
           count

           zip-index
           zip
           permutate
           nest
           take take-while
           drop drop-while
           slice
           map map-n map2
           flat-map
           filter
           filter-map

           for-list
           for-array
           for-string
           for-file-line
           for-hash-table
           for-hash-key
           for-hash-value
           range))
(in-package :loop)

  