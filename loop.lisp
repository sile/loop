(in-package :loop)

;; TODO: defgenerator
;; TODO: defmapper

(declaim (inline range))
(defun range (start end &key (by 1))
  (lambda ()
    (values start
            (lambda (cur) (+ cur by))
            (lambda (cur) (> cur end))
            (lambda (fn) fn))))

(declaim (inline filter))
(defun filter (filter-fn loop)
  (declare (function filter-fn loop))
  (multiple-value-bind (start update-fn end-fn apply-fn) (funcall loop)
    (declare (function update-fn end-fn apply-fn))
    (lambda ()
      (values start
              update-fn
              end-fn
              (lambda (fn)
                (funcall apply-fn (lambda (cur) 
                                    (when (funcall filter-fn cur)
                                      (funcall fn cur)))))
              ))))

(declaim (inline map))
(defun map (map-fn loop)
  (declare (function map-fn loop))
  (multiple-value-bind (start update-fn end-fn apply-fn) (funcall loop)
    (declare (function update-fn end-fn apply-fn))
    (lambda ()
      (values start
              update-fn
              end-fn
              (lambda (fn)
                (funcall apply-fn (lambda (cur) (funcall fn (funcall map-fn cur)))))
              ))))
  
(declaim (inline each))
(defun each (fn loop)
  (declare (function loop))
  (multiple-value-bind (start update-fn end-fn apply-fn) (funcall loop)
    (declare (function update-fn end-fn apply-fn))
    (do ((cur start (funcall update-fn cur)))
        ((funcall end-fn cur))
      (funcall (funcall apply-fn fn) cur))))

(declaim (inline reduce))
(defun reduce (fn init loop)
  (let ((acc init))
    (each (lambda (x)
            (setf acc (funcall fn acc x)))
          loop)
    acc))

(declaim (inline collect))
(defun collect (loop)
  (nreverse (reduce (lambda (acc x) (cons x acc))
                    '()
                    loop)))

;;;;;;;;;;
#+C
(defmacro range (start end &key (by 1) reverse)
  (if (not reverse)
      `(values ',start
               '(lambda (cur) (+ cur ,by))
               '(lambda (cur) (> cur ,end))
               '(lambda (cur) (declare (ignore cur)) t)
               '(lambda (cur) cur))
    `(values ',start
             '(lambda (cur) (- cur ,by))
             '(lambda (cur) (< cur ,end))
             '(lambda (cur) (declare (ignore cur)) t)
             '(lambda (cur) cur))))
#|
;;;;;;;;;;;;;
(defmacro filter (fn loop-exp)
  (multiple-value-bind (start update-fn end-fn filter-fn map-fn) (eval loop-exp)
    `(values ',start
             ',update-fn
             ',end-fn
             '(lambda (cur)
                (and (,filter-fn cur)
                     (,fn cur)))
             ',map-fn)))

(defmacro map (fn loop-exp)
  (multiple-value-bind (start update-fn end-fn filter-fn map-fn) (eval loop-exp)
    `(values ',start
             ',update-fn
             ',end-fn
             ',filter-fn
             '(lambda (cur) (,fn (,map-fn cur))))))
      
                     
;;;;;;;;;;;;;
(defmacro each (fn loop-exp)
  (multiple-value-bind (start update-fn end-fn filter-fn map-fn) (eval loop-exp)
    (let ((cur (gensym)))
      `(do ((,cur ,start (,update-fn ,cur)))
         ((,end-fn ,cur))
         (when (,filter-fn ,cur)
           (,fn (,map-fn ,cur)))))))

(defmacro reduce (fn init loop-exp)
  (let ((acc (gensym))
        (x   (gensym)))
    `(let ((,acc ,init))
       (each (lambda (,x)
               (setf ,acc (,fn ,acc ,x)))
             ,loop-exp)
       ,acc)))

(defmacro collect (loop-exp)
  `(nreverse (reduce (lambda (acc x) (cons x acc))
                     '()
                     ,loop-exp)))
|#             
#|
(deftype callback-function () '(function (t) (values)))
(deftype one-arg-function () '(function (t) (values t)))
(deftype two-arg-function () '(function (t t) (values t)))
(deftype positive-fixnum () '(integer 1 #.most-positive-fixnum))
(deftype non-negative-fixnum () '(integer 0 #.most-positive-fixnum))

(defmacro define (name args &body body)
  (let ((callback (gensym))
        (loop-block (gensym)))
    `(progn
       (declaim (inline ,name))
       (defun ,name ,args
         (macrolet ((yield (x)
                      `(funcall ,',callback ,x))
                    (break-loop ()
                      `(return-from ,',loop-block)))
           (lambda (,callback)
             (declare (callback-function ,callback))
             (block ,loop-block
               (locally ,@body))))))))

(define range (start end &key (by 1))
  (declare (fixnum start end)
           (positive-fixnum by))
  (if (< start end)
      (loop FOR i fixnum FROM start TO end BY by 
            DO (yield i))
    (loop FOR i fixnum FROM start DOWNTO end BY by
          DO (yield i))))


(define for-list (list)
  (dolist (x list)
    (yield x)))

(define for-array (array)
  (loop FOR x ACROSS array
        DO (yield x)))

(define for-string (string)
  (loop FOR x ACROSS string
        DO (yield x)))

(define for-file-line (path)
  (with-open-file (in path)
    (loop FOR line = (read-line in nil nil)
          WHILE line
          DO (yield line))))

(define for-hash-table (hash-table)
  (maphash (lambda (key value)
             (yield (list key value)))
           hash-table))

(define for-hash-key (hash-table)
  (maphash (lambda (key value)
             (declare (ignore value))
             (yield key))
           hash-table))

(define for-hash-value (hash-table)
  (maphash (lambda (key value)
             (declare (ignore key))
             (yield value))
           hash-table))

(define take (n loop)
  (declare (non-negative-fixnum n)
           (callback-function loop))
  (let ((i -1))
    (each (lambda (x)
            (when (= (incf i) n)
              (break-loop))
            (yield x))
          loop)))

(defun slice (start end loop)
  (take (- end start) (drop start loop)))

(define drop (n loop)
  (declare (non-negative-fixnum n))
  (let ((i 0))
    (each (lambda (x)
            (if (< i n)
                (incf i)
              (yield x)))
          loop)))

(define map (fn loop)
  (declare (callback-function loop)
           (one-arg-function fn))
  (each (lambda (x)
          (yield (funcall fn x)))
        loop))

(define flat-map (fn loop)
  (declare (one-arg-function fn))
  (each (lambda (x)
          (dolist (y (funcall fn x))
            (yield y)))
        loop))

(define filter (fn loop)
  (declare (callback-function loop)
           (one-arg-function fn))
  (each (lambda (x)
          (when (funcall fn x)
            (yield x)))
        loop))

;; XXX: in-efficient
(define zip (loop &rest loops)
  (let ((lists (mapcar #'collect loops)))
    (each (lambda (x)
            (when (some #'null lists)
              (break-loop))
            (yield (cons x (mapcar #'car lists)))
            (setf lists (mapcar #'cdr lists)))
          loop)))

(define zip-index (loop)
  (let ((i -1))
    (declare (fixnum i))
    (each (lambda (x)
            (yield (list (incf i) x)))
          loop)))

;; XXX: name
;; TODO: macro
(define nest (loop &rest loops)
  (labels ((recur (args rest-loop)
             (if (null rest-loop)
                 (yield (reverse args))
               (each (lambda (x)
                       (recur (cons x args) (cdr rest-loop)))
                     (car rest-loop)))))
    (each (lambda (x)
            (recur (list x) loops))
          loop)))

(declaim (inline each))
(defun each (fn loop)
  (declare (callback-function fn loop))
  (funcall loop fn))

(declaim (inline reduce))
(defun reduce (fn init loop &aux (acc init))
  (declare (callback-function loop)
           (two-arg-function fn))
  (each (lambda (x)
          (setf acc (funcall fn acc x)))
        loop)
  acc)

(declaim (inline collect))
(defun collect (loop) ;; TODO: defXXX を提供して拡張可能なようにする
  (nreverse
   (reduce (lambda (acc x) (cons x acc)) '() loop)))

(declaim (inline all? any?))
(defun all? (fn loop)
  (declare (one-arg-function fn))
  (each (lambda (x)
          (unless (funcall fn x)
            (return-from all? nil)))
        loop)
  t)

(defun any? (fn loop)
  (declare (one-arg-function fn))
  (each (lambda (x)
          (when (funcall fn x)
            (return-from any? t)))
        loop)
  nil)

(declaim (inline max/min-impl max min))
(defun max/min-impl (loop key test)
  (declare (one-arg-function key)
           (two-arg-function test))
  (let ((selected-value   nil)
        (selected-element nil))
    (each (lambda (x)
            (let ((value (funcall key x)))
              (when (or (null selected-value)
                        (funcall test value selected-value))
                (setf selected-value value
                      selected-element x))))
          loop)
    (values selected-value selected-element)))
  
(defun max (loop &key (key #'identity))
  (max/min-impl loop key (lambda (x y) (> x y))))

(defun min (loop &key (key #'identity))
  (max/min-impl loop key (lambda (x y) (< x y))))

(declaim (inline sum))
(defun sum (loop &key (key #'identity))
  (declare (one-arg-function key))
  (reduce (lambda (total x)
            (+ total (funcall key x)))
          0
          loop))
  
(declaim (inline count))
(defun count (fn loop)
  (declare (one-arg-function fn))
  (reduce (lambda (count x)
            (if (funcall fn x)
                (1+ count)
              count))
          0
          loop))
|#
