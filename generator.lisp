(in-package :loop)

(defmacro make-generator (&key init next end? (key '#'identity))
  (let ((cur (gensym)))
    `(lambda ()
       (let ((,cur (funcall ,init)))
         (values (lambda () (setf ,cur (funcall ,next ,cur)))
                 (lambda () (funcall ,end? ,cur))
                 (lambda (fn) (funcall fn (funcall ,key ,cur))))))))

(defmacro def-generator (name args &key init next end? (key '#'identity))
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args
       (make-generator :init ,init :next ,next :end? ,end? :key ,key))))

(def-generator from (start &key to (by 1))
  :init (lambda () start)
  :next (lambda (cur) (+ cur by))
  :end? (lambda (cur) (and to (> cur to))))

(def-generator down-from (start &key to (by 1))
  :init (lambda () start)
  :next (lambda (cur) (- cur by))
  :end? (lambda (cur) (and to (< cur to))))

(defmacro for-list (list &key (element-type t))
  (let ((start (gensym)))
    `(let ((,start ,list))
       (make-generator :init (lambda () ,start)
                       :next #'cdr
                       :end? #'endp
                       :key (lambda (x) (the ,element-type (car x)))))))

(def-generator for-string (str &key (start 0) (end (length str)))
  :init (lambda () start)
  :next (lambda (cur) (1+ cur))
  :end? (lambda (cur) (>= cur end))
  :key  (lambda (cur) (char str cur)))

(def-generator for-array (ary &key (start 0) (end (length ary)))
  :init (lambda () start)
  :next (lambda (cur) (1+ cur))
  :end? (lambda (cur) (>= cur end))
  :key  (lambda (cur) (aref ary cur)))

(def-generator repeat (fn)
  :init (lambda () (funcall fn))
  :next (lambda (cur) (declare (ignore cur)) (funcall fn))
  :end? (lambda (cur) (declare (ignore cur)) nil))
