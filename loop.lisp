(in-package :loop)

;; TODO: defgenerator
;; TODO: defmapper

(declaim (inline range))
(defun range (start end &key (by 1))
    (lambda ()
      (let ((cur start))
        (values (lambda () (incf cur by))
                (lambda () (> cur end))
                (lambda (fn) (funcall fn cur))))))

(defmacro add-apply (loop apply-fn) ; => add-apply-hook
  `(lambda ()
     (multiple-value-bind (update-fn end-fn apply-fn) (funcall (the function ,loop))
       (declare (function update-fn end-fn apply-fn))
       (values update-fn
               end-fn
               (lambda (fn)
                 (macrolet ((next (arg) `(funcall fn ,arg)))
                   (funcall apply-fn ,apply-fn)))))))
       
(declaim (inline filter))
(defun filter (filter-fn loop)
  (declare (function filter-fn))
  (add-apply loop (lambda (cur)
                    (when (funcall filter-fn cur)
                      (next cur)))))

(declaim (inline map))
(defun map (map-fn loop)
  (declare (function map-fn))
  (add-apply loop (lambda (cur)
                    (next (funcall map-fn cur)))))

(defmacro map-n (arity map-fn loop &aux (args (loop REPEAT arity COLLECT (gensym))))
  `(add-apply ,loop (lambda ,args
                      (next (funcall ,map-fn ,@args)))))

(declaim (inline filter-map))
(defun filter-map (filter-map-fn loop)
  (declare (function filter-map-fn))
  (add-apply loop (lambda (cur)
                    (multiple-value-bind (val ok?) (funcall filter-map-fn cur)
                      (when ok?
                        (next val))))))

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


  
(declaim (inline each))
(defun each (fn loop) 
  (declare (function loop))
  (multiple-value-bind (update-fn end-fn apply-fn) (funcall loop)
    (declare (function update-fn end-fn apply-fn))
    (loop UNTIL (funcall end-fn)
          DO (funcall apply-fn fn)
             (funcall update-fn))))

(declaim (inline reduce))
(defun reduce (fn init loop)
  (let ((acc init))
    (each (lambda (x)
            (setf acc (funcall fn acc x)))
          loop)
    acc))

(defmacro reduce2 (args init loop &rest body)
  `(let ((acc ,init))
     (each (lambda ,args
             (setf acc (locally ,@body)))
           ,loop)
     acc))

(declaim (inline collect))
(defun collect (loop)
  (nreverse (reduce (lambda (acc x) (cons x acc))
                    '()
                    loop)))
