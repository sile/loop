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

(declaim (inline filter-map))
(defun filter-map (filter-map-fn loop)
  (declare (function filter-map-fn))
  (add-apply loop (lambda (cur)
                    (multiple-value-bind (val ok?) (funcall filter-map-fn cur)
                      (when ok?
                        (next val))))))

(declaim (inline zip)) ;; TODO: zip-n, map-n
(defun zip (loop1 loop2 &aux (undef (gensym)))
  (declare (function loop1 loop2))
  (lambda ()
    (multiple-value-bind (update-fn1 end-fn1 apply-fn1) (funcall loop1)
      (multiple-value-bind (update-fn2 end-fn2 apply-fn2) (funcall loop2)
        (let ((memo1 undef)
              (memo2 undef))
          (values (lambda ()
                    (when (eq memo1 undef) (funcall update-fn1))
                    (when (eq memo2 undef) (funcall update-fn2)))
                  
                  (lambda ()
                    (or (funcall end-fn1)
                        (funcall end-fn2)))
                  
                  (lambda (fn)
                    (when (eq memo1 undef)
                      (funcall apply-fn1 (lambda (val) (setf memo1 val))))
                    
                    (when (eq memo2 undef)
                      (funcall apply-fn2 (lambda (val) (setf memo2 val))))
                    
                    (when (not (or (eq memo1 undef)
                                   (eq memo2 undef)))
                      (funcall fn memo1 memo2)
                      (setf memo1 undef
                            memo2 undef)))
                  ))))))

(declaim (inline map2))
(defun map2 (map-fn loop1 loop2)
  (declare (function map-fn))
  (add-apply (zip loop1 loop2)
             (lambda (val1 val2)
               (next (funcall map-fn val1 val2)))))

;; TODO: map-n
  
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
