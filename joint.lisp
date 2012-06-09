(in-package :loop)

(eval-when (:compile-toplevel :load-toplevel)
  (defun gensyms (n) (loop REPEAT n COLLECT (gensym))))
  
(defmacro make-joint (generator joint-fn &key end)
  (let ((update-fn (gensym))
        (end-fn    (gensym))
        (apply-fn  (gensym))
        (callback-fn (gensym)))
  `(lambda ()
     (multiple-value-bind (,update-fn ,end-fn ,apply-fn)
                          (funcall (the function ,generator))
       (declare (function ,update-fn ,end-fn ,apply-fn))
       (values ,update-fn
               ,(if (null end)
                    end-fn
                  `(lambda ()
                     (flet ((call-next () (funcall ,end-fn)))
                       (declare (inline call-next))
                       (funcall ,end))))
               (lambda (,callback-fn)
                 (macrolet ((call-next (&rest args)
                              `(funcall (the function ,',callback-fn) ,@args)))
                   (funcall ,apply-fn ,joint-fn))))))))

(defmacro map-n (arity map-fn generator)
  (let ((args (gensyms arity)))
    `(make-joint ,generator 
                 (lambda ,args
                   (call-next (funcall ,map-fn ,@args))))))

(declaim (inline map))
(defun map (map-fn generator) (map-n 1 map-fn generator))


(defmacro filter-n (arity filter-fn generator)
  (let ((args (gensyms arity)))
    `(make-joint ,generator
                 (lambda ,args
                   (when (funcall ,filter-fn ,@args)
                     (call-next ,@args))))))

(declaim (inline filter))
(defun filter (filter-fn generator) (filter-n 1 filter-fn generator))


(defmacro take-n (arity n generator)
  (let ((args (gensyms arity))
        (i     (gensym))
        (limit (gensym)))
    `(let ((,i 0)
           (,limit ,n))
       (make-joint ,generator
                   (lambda ,args
                     (incf ,i)
                     (call-next ,@args))

                   :end (lambda () (or (>= ,i ,limit) (call-next)))))))

(declaim (inline take))
(defun take (n generator) (take-n 1 n generator))

(defmacro take-while-n (arity pred-fn generator)
  (let ((args (gensyms arity))
        (finish (gensym)))
    `(let ((,finish nil))
       (make-joint ,generator
                   (lambda ,args
                     (setf ,finish (not (funcall ,pred-fn ,@args)))
                     (unless ,finish
                       (call-next ,@args)))
                   :end (lambda () (or ,finish (call-next)))))))

(declaim (inline take-while))
(defun take-while (pred-fn generator) (take-while-n 1 pred-fn generator))

(defmacro drop-n (arity n generator)
  (let ((args (gensyms arity))
        (i     (gensym))
        (border (gensym)))
    `(let ((,i 0)
           (,border ,n))
       (make-joint ,generator
                   (lambda ,args
                     (if (< ,i ,border)
                         (incf ,i)
                       (call-next ,@args)))))))

(declaim (inline drop))
(defun drop (n generator) (drop-n 1 n generator))

(defmacro drop-while-n (arity pred-fn generator)
  (let ((args (gensyms arity))
        (drop? (gensym)))
    `(let ((,drop? t))
       (make-joint ,generator
                   (lambda ,args
                     (if ,drop?
                         (unless (setf ,drop? (funcall ,pred-fn ,@args))
                           (call-next ,@args))
                       (call-next ,@args)))))))

(declaim (inline drop-while))
(defun drop-while (pred-fn generator) (drop-while-n 1 pred-fn generator))

;; TODO: 整理
(defmacro zip (loop1 loop2 &rest loops)
  (let ((undef (gensym "UNDEF"))
        (loops (append (list loop1 loop2) loops))
        (fn (gensym "FN")))
    (labels ((gen-zip-body (loops vars-list)
               (if (null loops)
                   (gen-body2 (reverse vars-list))
                 (let ((vars (list (gensym "MEMO") (gensym "UPDATE") (gensym "END") (gensym "APPLY"))))
                   `(multiple-value-bind ,(cdr vars) (funcall ,(car loops))
                      (let ((,(car vars) ,undef))
                        ,(gen-zip-body (cdr loops) (cons vars vars-list)))))))
             (gen-body2 (vars-list)
               `(values (lambda ()
                          ,@(loop FOR (memo update-fn) IN vars-list
                                  COLLECT `(when (eq ,memo ,undef) (funcall ,update-fn))))
                        
                        (lambda ()
                          (or ,@(loop FOR (_ __ end-fn) IN vars-list
                                      COLLECT `(funcall ,end-fn))))

                        (lambda (,fn)
                          ,@(loop FOR (memo _ __ apply-fn) IN vars-list
                                  COLLECT `(when (eq ,memo ,undef)
                                             (funcall ,apply-fn (lambda (val) (setf ,memo val)))))
                          (when (not (or ,@(loop FOR (memo) IN vars-list
                                                 COLLECT `(eq ,memo ,undef))))
                            (funcall ,fn ,@(mapcar #'car vars-list))
                            (setf ,@(loop FOR (memo) IN vars-list
                                          APPEND (list memo undef)))))
                        )))
      `(lambda (&aux (,undef (gensym)))
         ,(gen-zip-body loops '())))))