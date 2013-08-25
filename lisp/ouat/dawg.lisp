(in-package :ouat)

(defmacro node-letter (node) `(caar ,node))
(defmacro node-number (node) `(cadar ,node))
(defmacro node-termination (node) `(cddar ,node))
(defmacro node-children (node) `(cdr ,node))

(defvar *data-directory* nil)
(defvar *test-directory* nil)
(defvar *default-dictionary* nil)

(defvar *twl-dawg-nodes* nil)
;(defvar *twl98-dawg* nil)
;(defvar *sowpods-dawg* nil)
;(defvar *csw-dawg* nil)

(defun node-child-which-has-letter (node letter)
  (find-if #'(lambda (child)
               (= letter (node-letter child)))
           (node-children node)))

(defun node-child-which-has-letter! (node letter)
  (or (node-child-which-has-letter node letter)
      (first (push (list (list letter nil)) (node-children node)))))

(defun push-suffix (suffix node)
  (setf (node-number node) 0)
  (let* ((letter (first suffix))
         (new-suffix (rest suffix)))
    (if suffix
        (push-suffix new-suffix (node-child-which-has-letter! node letter))
        (setf (node-termination node) t))))

(defun push-word (word trie)
  (push-suffix (coerce word 'list) trie))

(defun reverse-children (node)
  (setf (node-children node) (nreverse (node-children node)))
  (dolist (child (node-children node))
    (reverse-children child)))

(defun node-child-bits (node)
  (let ((child-bits 0))
    (dolist (child (node-children node))
      (setf child-bits (logior child-bits (1bit (node-letter child)))))
    child-bits))

(defun node-bits (node)
  (let ((bits (node-number node)))
    (when (node-termination node)
      (setf bits (logior bits (1bit 26))))
    bits))

(defun number-trie (node)
  (let ((index 0))
    (labels ((number-subtrie (node)
               (if (node-children node)
                   (progn
                     (setf (node-number node) index)
                     (incf index)
                     (incf index (length (node-children node)))
                     (dolist (child (node-children node))
                       (number-subtrie child)))
                   (setf (node-number node) 0))))
      (number-subtrie node))
    index))

(defun trie-from-file (filename)
  (let ((trie (list (list nil nil))))
    (with-open-file (stream filename)
      (loop for word = (read-line stream nil) while word
         do (let ((word-list (word2list word)))
              (push-word word-list trie)))
      trie)))

(defun dawg-from-wordlist (wordlist dawg)
   (let* ((trie (trie-from-file wordlist))
          (size (progn (reverse-children trie) (number-trie trie))))
    (with-open-file (stream
                     dawg
                     :direction :output
                     :element-type '(unsigned-byte 32)
                     :if-does-not-exist :create)
      (labels ((write-node (node)
                 (when (node-children node)
                   (write-byte (node-child-bits node) stream)
                   (dolist (child (node-children node))
                     (write-byte (node-bits child) stream))
                   (dolist (child (node-children node))
                     (write-node child)))))
        (write-byte size stream)
        (write-node trie)))))

(defmacro open-dawg (array dawg-filename)
  `(with-open-file (stream
                    ,dawg-filename
                    :element-type '(unsigned-byte 32))
     (let ((size (read-byte stream)))
       (setf ,array (make-array `(,size) :element-type 'fixnum))
       (dotimes (index size)
         (setf (aref ,array index) (read-byte stream))))))

(defun node-letter-offset (node letter)
  (declare (type (simple-array fixnum) *one-bits-per-13* *mask*))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((masked (ilogand node (mask letter)))
         (first-13 (ilogand masked (mask 13)))
         (second-13 (iash masked -13)))
    (i+ 1
        (aref *one-bits-per-13* first-13)
        (aref *one-bits-per-13* second-13))))

(defun node-has-child-letter (node letter)
  (not (izerop (ilogand node (1bit letter)))))

(defmacro do-node-child-letters ((letter node bits counts) &body body)
  `(loop 
      for letters-to-search = (if (iplusp (aref ,counts +blank+))
                                  ,node
                                  (ilogand ,node ,bits))
      for ,letter fixnum below +alphabet-size+
      when (iplusp (ilogand letters-to-search (1bit ,letter)))
      do ,@body))
        
(defun child-pointer (child)
  (ilogand child (mask 26)))

(defun child-terminates (child)
  (not (izerop (ilogand child (1bit 26)))))

(defun anagram (anagram-string &optional (max-words 1)
		                         (dictionary *default-dictionary*)
		                         (return-values '(:WORDS))
		                         (concatenate-p t)
                                         (build-p nil))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (declare (type (simple-array fixnum) *bits* *other-bits* dictionary))
  (let ((word-results nil)
	(blank-results nil)
        (word-prefix nil)
	(blank-prefix nil)
        (bits (word-bits anagram-string))
        (counts (word-set anagram-string))
        (count (num-tiles (word-set anagram-string)))
        (number-of-answers 0))
    (declare (type (simple-array fixnum) counts))
    (labels ((push-words-starting-at-index (index words-so-far)
               (let* ((node (aref dictionary index)))
                 (do-node-child-letters (letter node bits counts)
                   (let* ((tile (cond ((iplusp (aref counts letter)) letter)
                                      ((iplusp (aref counts +blank+)) +blank+)
                                      (t nil))))
                     (when tile
                       (let* ((child-index (i+ index (node-letter-offset node letter)))
                              (child (aref dictionary child-index))
                              (pointer (child-pointer child)))
                         (labels ((recordable-word-here ()
                                    (and (child-terminates child)
                                         (or (i= 1 count) build-p))))

                           (when (recordable-word-here)
                             (when (member :WORDS return-values)
                               (push (list2word (cons letter word-prefix)) word-results))
                           
                             (when (member :BLANKS return-values)
                               (push (list2word
                                      (sort
                                       (copy-list (if (i= +blank+ tile)
                                                      (cons letter blank-prefix)
                                                      (copy-list blank-prefix)))
                                       '>))
                                     blank-results))

                             (when (member :NUMBER-OF-ANSWERS return-values)
                               (iincf number-of-answers)))

                           (when (member :WORDS return-values)
                             (push letter word-prefix))

                           (when (and (member :BLANKS return-values)
                                      (i= +blank+ tile))
                             (push letter blank-prefix))

                           (idecf (aref counts tile))
                           (when (izerop (aref counts tile))
                             (unset-bitf bits letter))
                           (idecf count)
                           
                           (when (and (child-terminates child)
                                      (i< (i1+ words-so-far) max-words))
                             (unless concatenate-p
                               (push +space+ word-prefix))
                             (push-words-starting-at-index 0 (i1+ words-so-far))
                             (unless concatenate-p
                               (pop word-prefix)))
                           
                           (unless (izerop pointer)
                             (push-words-starting-at-index pointer words-so-far))
                           
                           (iincf count)
                           (when (izerop (aref counts tile))
                             (set-bitf bits letter))
                           (iincf (aref counts tile))
                           
                           (when (member :WORDS return-values)
                             (pop word-prefix))
                           
                           (when (member :BLANKS return-values)
                             (when (i= +blank+ tile)
                               (pop blank-prefix)))))))))))

      (push-words-starting-at-index 0 0)

      (cond
	((equal '(:NUMBER-OF-ANSWERS) return-values)
         number-of-answers)
        ((equal '(:WORDS) return-values)
	 (nreverse word-results))
	((equal '(:BLANKS) return-values)
	 (nreverse blank-results))
	((equal '(:WORDS :BLANKS) return-values)
	 (values
	  (nreverse word-results)
	  (nreverse blank-results)))))))

(defun ana (anagram-string)
  (anagram anagram-string))

(defun steal-tiles (anagram-string)
  (anagram anagram-string
	   1
	   *default-dictionary*
	   '(:BLANKS)))

(defun anagram-stem (anagram-string)
  (anagram anagram-string
	   1
	   *default-dictionary*
	   '(:WORDS :BLANKS)))

(defun load-dictionaries ()
  (if *data-directory*
      (open-dawg *twl-dawg-nodes* (string-append *data-directory*
                                           "twl.dawg"))
      (format t "No data directory set in ~~/.ouatrc, ~
                 couldn't find data.")))

(load (format nil "~a~a" (user-homedir-pathname) ".ouatrc"))
