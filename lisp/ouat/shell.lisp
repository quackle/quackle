(in-package :ouat)

#+nil
(defvar command-list
  '(("a" . (ana 1 nil))
    ("d" . (defi 1 nil))
    ("h" . (help 0 nil))
    ("l" . (by-length 0 t))
    ("p" . (seven-prob 1 nil))
    ("q" . (quit-words 0 nil))))

(defvar *command-list*
  '(("a" . ana)
    ("b" . build)
    ("q" . quit-words)
    ("w" . woadwage)))

(defun quit-words (arg)
  (declare (ignore arg))
  :QUIT)

(defun woadwage (arg)
  (anagram arg 2))

(defun build (arg)
  (anagram arg 1 *default-dictionary* '(:WORDS) t t))

(defun build-num (arg)
  (anagram arg 1 *default-dictionary* '(:NUMBER-OF-ANSWERS) t t))

(defun num-anagrams (arg)
  (anagram arg 1 *default-dictionary* '(:NUMBER-OF-ANSWERS)))

(defun handle (query)
  (when (< 1 (length (first query)))
    (push "a" query))
  (let ((command (first query))
        (arg (second query))
        (subquery (cddr query)))
    (declare (ignore subquery))
    (apply (cdr (assoc command *command-list* :test 'equal)) (list arg))))

(defun words ()
  (loop 
   do
   (let ((result (handle (string-tokens (read-line)))))
     (when (eq result :QUIT) (return))
     (format t "~a~%" result)))
  (values))