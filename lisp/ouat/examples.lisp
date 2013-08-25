(in-package :ouat)

(defun jqxz+1-stems ()
  (let ((racks-that-bingo-through-jqxz
         (unique
          (loop
             for big-tile across "JQXZ"
             for ana-pattern = (format nil "~a???????" big-tile)
             append (steal-tiles ana-pattern))))
        (jqxz+1-stems nil))
    (dolist (rack racks-that-bingo-through-jqxz)
      (let ((ana-pattern (format nil "~a?" rack)))
        (multiple-value-bind (words blanks)
            (anagram-stem ana-pattern)
          (when (and (= 2 (length words))
                     (string/= (first blanks) 
                               (second blanks)))
            (push rack jqxz+1-stems)))))
    jqxz+1-stems))  

#+nil
(defun woadwage-bingos (n)
  (let ((bag (coerce *english-scrabble-bag* 'list)))
    (labels ((draw-random-rack ()
               (coerce (subseq (shuffle bag) 0 7) 'string)))
      (loop
         for i below n
         for rack = (draw-random-rack)
         count (woadwage rack)))))

(defun chewscore (word1 word2)
  (labels ((uniq-pairs (word)
             (unique
              (sort 
               (loop
                  for idx below (1- (length word))
                  collect (subseq word idx (+ 2 idx)))
               'string<))))
  (let* ((pairs1 (uniq-pairs word1))
         (pairs2 (uniq-pairs word2)))
    (- (length (union pairs1 pairs2 :test 'string=))
       (max (length pairs1) (length pairs2))))))

(defun wordmath ()
  (declare (optimize (debug 3)))
  (let ((twl-alphas (make-hash-table :test 'equal))
        (csw-alphas (make-hash-table :test 'equal)))
    (labels ((alpha (word)
               (sort (coerce word 'list) 'char<))
             (minus-one (word+already-subtracted)
               #+nil
               (format t "(minus-one ~a)~%" word+already-subtracted)
               (loop
                  with word = (first word+already-subtracted)
                  with already-subtracted = (rest word+already-subtracted)
                  with used-letters = (make-hash-table)
                  for idx below (length word)
                  for letter = (elt word idx)
                  for before = (subseq word 0 idx)
                  for after = (subseq word (1+ idx))
                  for alpha = (coerce 
                               (alpha (concatenate 'string before after))
                               'string)
                  unless (gethash letter used-letters) do
                    #+nil
                    (format t "alpha: ~a~%" alpha)
                    (setf (gethash letter used-letters) t)
                    collect (cons alpha (cons letter already-subtracted)))) 
             (best-subword (word num-missing)
               #+nil
               (format t "word: ~a~%" word)
               (loop
                  with sub-alphas = (list (list word))
                  repeat num-missing
                  do
                    (setf sub-alphas
                          (unique (apply 'append (mapcar #'minus-one sub-alphas))))
                    #+nil
                    (format t "sub-alphas: ~a~%" sub-alphas)
                  finally
                    (loop
                       with best-score = MOST-NEGATIVE-FIXNUM
                       with best = nil
                       for (alpha . letters) in sub-alphas
                       for a = (coerce alpha 'list)
                       for twl = (gethash a twl-alphas)
                       for csw = (gethash a csw-alphas)
                       ;;do (format t "twl: ~a csw: ~a~%" twl csw)
                       when (and twl (= 1 (length csw))) do
                         (let* ((subword (first csw))
                                (score (chewscore subword word)))
                           #+nil
                           (format t "subword: ~a letters: ~a score: ~a~%" subword letters score)
                           (when (> score best-score)
                             (setf best-score score
                                   best (cons subword letters))))
                       finally
                         (when best
                           #+nil
                           (format t "BEST: ~a~%" best)
                           (return-from best-subword
                             (values (first best) (rest best) best-score)))))))
      (do-file-lines (word (string-append *data-directory* "twl.txt"))
        (when (<= 5 (length word) 7)
          (push word (gethash (alpha word) twl-alphas))))
      (do-file-lines (word (string-append *data-directory* "csw.txt"))
        (when (<= 5 (length word) 7)
          (push word (gethash (alpha word) csw-alphas))))
      (do-file-lines (word (string-append *data-directory*
                                          "csw-single-sevens.txt"
                                          #+nil
                                          "csw-single-eights.txt"))
        (multiple-value-bind (subword letters score)
            (best-subword word 1)
          #+nil
          (format t "subword: ~a~%" subword)
          (if (and score (> score 2))
              (format t "~a + ~a = ~a~%" subword (first letters) word)
              (multiple-value-bind (subword2 letters2 score2)
                  (best-subword word 2)
                #+nil
                (format t "subword: ~a~%" subword)
                (when (and score2 (> score2 2))
                  (format t "~a + ~a = ~a~%"
                          subword2
                          (concatenate 'string (sort letters2 'char<))
                          word)))))))))

(defun csw-hook-worksheet (length words-shown hooks-shown columns hidden)
  (declare (optimize (debug 3)))
  (let ((front (make-hash-table :test 'equal))
        (back (make-hash-table :test 'equal))
        (words nil)
        (csw (make-hash-table :test 'equal))
        (twl (make-hash-table :test 'equal))
        (hook-cols (floor (- columns length 3) 2)))
    (do-file-lines (word (string-append *data-directory* "twl.txt"))
      (setf (gethash word twl) t))
    (do-file-lines (word (string-append *data-directory* "csw.txt"))
      (when (and (= length (length word))
                 (not (gethash word twl)))
        (push word words))
      (setf (gethash word csw) t))
    (setf words (nreverse words))
    (do-file-lines (word (string-append *data-directory* "csw.txt"))
      (when (= (1+ length) (length word))
        (let ((left-substr (subseq word 0 length))
              (left-letter (elt word 0))
              (right-substr (subseq word 1))
              (right-letter (elt word length)))
          (when (gethash right-substr csw)
            (push (if (gethash word twl)
                      (char-downcase left-letter)
                      left-letter)
                  (gethash right-substr front)))
          (when (gethash left-substr csw)
            (push (if (gethash word twl)
                      (char-downcase right-letter)
                      right-letter)
                  (gethash left-substr back))))))
    (dolist (word words)
      (labels ((maybe-hide (letter)
                 (case hooks-shown
                   (:all letter)
                   (:twl (if (char= letter (char-downcase letter))
                             letter
                             #\_))
                   (nil #\_))))
        (let* ((raw-fronts (nreverse (gethash word front)))
               (fronts (coerce (mapcar #'maybe-hide raw-fronts) 'string))
               (front-rows (ceiling (length fronts) hook-cols))
               (raw-backs (nreverse (gethash word back)))
               (backs (coerce (mapcar #'maybe-hide raw-backs) 'string))
               (back-rows (ceiling (length backs) hook-cols)))
          (loop
             for row below (max 1 front-rows back-rows)
             for start = (* row hook-cols)
             for end = (+ start hook-cols)
             for front-row = (subseq fronts
                                     (min start (length fronts))
                                     (min end (length fronts)))
             for back-row = (subseq backs
                                    (min start (length backs))
                                    (min end (length backs)))
             for padding = (- hook-cols (length front-row))
             do
               (loop repeat padding do (format t " "))
               (format t "~a " front-row)
               (if (zerop row)
                   (if words-shown
                       (format t "~a#" word)
                       (progn
                         (format t "~a" (subseq word 0 (- length hidden)))
                         (loop repeat hidden do (format t "_"))
                         (format t "#")))
                   (loop repeat (1+ length) do (format t " ")))
               (format t " ~a~%" back-row)))))))
