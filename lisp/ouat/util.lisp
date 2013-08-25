(in-package :ouat)

(defvar *alphabet*
  (loop
     for letter from (char-code #\A) to (char-code #\Z)
     for index from 0
     collect index))

(defvar +alphabet-size+ (length *alphabet*))

(defvar *bits* (make-array '(28)))

(defvar *other-bits* (make-array '(28)))

(defconstant +blank+ 26)
(defconstant +star+ 27)
(defconstant +space+ 28)

(defmacro 1bit (x) 
  `(aref *bits* ,x))

(defmacro other-bits (x) 
  `(aref *other-bits* ,x))

(dotimes (x 28) (setf (1bit x) (ash 1 x)))
(dotimes (x 28) (setf (other-bits x) (ilognot (1bit x))))

(defun letter2char (letter)
  (if (i= +space+ letter)
      #\Space
      (code-char (+ letter (char-code #\A)))))

(defun word2list (word)
  (mapcar #'(lambda (x) (- (char-code x) (char-code #\A)))
          (coerce word 'list)))

(defun format-bits (bits)
  (coerce 
   (loop 
      for i from 0 to 25 
      append (unless (zerop (logand bits (1bit i)))
               (list (letter2char i))))
   'string))

(defun letter-int (letter)
  (cond
   ((eq letter #\?) +blank+)
   ((eq letter #\.) +blank+)
   ((eq letter #\*) +star+)
   ((eq letter #\/) +star+)
   ((and (>= (char-code letter) (char-code #\A))
         (<= (char-code letter) (char-code #\Z))) 
    (- (char-code letter) (char-code #\A)))
   ((and (>= (char-code letter) (char-code #\a))
         (<= (char-code letter) (char-code #\z))) 
    (- (char-code letter) (char-code #\a)))))

(defun word-list (word)
  (mapcar 'letter-int (coerce word 'list)))

(defun list2word (list)
  (coerce (mapcar #'letter2char (reverse list)) 'string))

(defun word-set (word)
  (let ((set (make-array '(28) :initial-element 0)))
    (dolist (letter (word-list word))
      (incf (aref set letter)))
    set))

(defun word-alist (word)
  (let ((set (word-set word)))
    (loop 
       for letter to +star+
       append (let ((number (aref set letter)))
                (unless (zerop number) 
                  (list (cons letter (aref set letter))))))))

(defun word-bits (word)
  (let ((word-bits 0)
        (set (word-set word)))
    (dolist (letter *alphabet*)
      (unless (zerop (aref set letter))
        (setf word-bits (logior word-bits (1bit letter)))))
  word-bits))

(defun num-tiles (set)
  (loop 
     for i from 0 to 26
     sum (aref set i)))

(defun reverse-length (list)
  (sort list '< :key (lambda (x) (length x))))

(defvar *one-bits-per-13* (make-array `(,(ash 1 13))))

(dotimes (x (ash 1 13))
  (setf (aref *one-bits-per-13* x)
        (loop for i from 0 to 12 counting (not (zerop (logand x (1bit i)))))))

(defvar *mask* (make-array '(28)))

(dotimes (x 28)
  (setf (aref *mask* x) (- (ash 1 x) 1)))

(defmacro mask (x) `(aref *mask* ,x))

(defun string-tokens (string)
  (labels ((get-token (str pos1 acc)
             (let ((pos2 (position #\Space str :start pos1)))
               (if (not pos2)
                   (nreverse acc)
                   (get-token str
                              (1+ pos2)
                              (cons (subseq str pos1 pos2)
                                    acc))))))
    (get-token (concatenate 'string string " ") 0 nil)))

(defun string-append (&rest strings)
  (coerce (apply 'append (mapcar #'(lambda (string) (coerce string 'list))
                                 strings))
          'string))

(defun unique (list)
  (remove-duplicates list :test 'equal))

(defmacro do-file-lines ((line filename) &body body)
  `(with-open-file (stream ,filename)
     (loop 
        for ,line = (read-line stream nil) while ,line
        do ,@body)))

(defun shuffle (seq)
  (map-into seq #'car
            (sort (map 'vector
                       (lambda (x)
                         (cons x (random MOST-POSITIVE-FIXNUM))) seq)
                  #'< :key #'cdr)))

(defmacro set-bitf (place b)
  `(declare ((type fixnum ,place ,b)))
  `(setf ,place (ilogior ,place (1bit ,b))))

(defmacro unset-bitf (place b)
  `(declare ((type fixnum ,place ,b)))
  `(setf ,place (ilogand ,place (other-bits ,b))))