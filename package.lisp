(defpackage loop ;; TODO: => flow?
  (:use :common-lisp)
  (:shadow :common-lisp reduce map max min count)
  (:export from down-from
           for-list for-string for-array
           for-hash-table
   collect
           each
           reduce
           all?
           any?
           max
           min
           sum
           count

           from

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

           for-file-line
           for-hash-key
           for-hash-value
           range))
(in-package :loop)

  