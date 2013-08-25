(in-package :ouat)

(defstruct blank-state
  idx
  limit
  rack-pos
  seq-idx
  assignment)

(defmacro def-move-finder (fn-name layout tiles
                           lexicon rules result-type debug-p)
 `(defun ,fn-name (board unsorted-rack bag)
   ,(if debug-p
       `(declare (optimize debug))
       `(declare (optimize (speed 3) (safety 0) (space 1) (debug 0))))
   (declare (ignore board bag)
            (type (simple-array tile) ,tiles))
   (let* ((rack (rack-sort unsorted-rack))
          (seq (unique-rack-perms rack))
          (elts (perm-seq-elts seq))
          (dawg (lexicon-dawg ,lexicon))
          (nodes (dawg-nodes dawg))
          (nth-child-letter-table (dawg-nth-child-letter-table dawg))
          (letter2child-table (dawg-letter2child-table dawg))
          (rack-pos-dawg-idxs (make-array (i1+ (rack-size rack))
                                          :element-type 'fixnum))
          (max-rack-pos (i1- (rack-size rack)))
          (seq-idx 0)
          (blank-states (make-array (i1+ (rack-size rack))))
          (move-over-blank-p nil)
          (blank-state nil)
          (num-blanks-active 0)
          (blank-id ,(tiles-blank (symbol-value tiles)))
          (blank-tile (aref ,tiles blank-id))
          (blank-quantity (tile-quantity blank-tile))
          (blank-designations (make-array blank-quantity :element-type 'fixnum))
          (word (make-array (the fixnum (rack-size rack)) :element-type 'fixnum))
          (letter-muls ,(layout-letter-muls (symbol-value layout)))
          (word-muls ,(layout-word-muls (symbol-value layout)))
          (center-row ,(layout-center-row (symbol-value layout)))
          (center-col ,(layout-center-col (symbol-value layout)))
          (bingo-bonus ,(rules-bingo-bonus (symbol-value rules)))
          (elt nil)
          (rack-pos nil)
          (letter-idx nil)
          (dawg-idx nil)
          (node 0)
          (skip-seq-idx nil)
          (tile-id nil)
          (letter-id +empty+)
          (length 0)
          (result ,(if (eq result-type :TOP-VALUE)
                       `MOST-NEGATIVE-FIXNUM
                      `nil)))

    (declare (ignore blank-designations)
             (type (simple-array perm-elt) elts)
             (type (simple-array fixnum) nodes)
             ;(type (simple-array fixnum) nth-child-letter-table)
             (type (simple-array (unsigned-byte 8)) nth-child-letter-table)
             (type (simple-array (unsigned-byte 32)) letter2child-table)
             (type fixnum node)
             (type (simple-array fixnum) rack-pos-dawg-idxs))

    ,(when debug-p
      `(format t "rack has ~a permutations~%"
               (length elts)))

    (loop while (i< seq-idx (length elts)) do
      (labels ((node-has-child (child-letter)
                 (logbitp (the fixnum child-letter) node))

               (find-child-idx (child-letter)
                 (when (node-has-child child-letter)
                   (i+ dawg-idx
                       (node-letter-offset node child-letter))))

               (fast-child-idx (idx child-letter)
                 (aref letter2child-table (i+ (i* 32 idx) child-letter)))
               
               (fast-nth-child-letter (idx n)
                 (aref nth-child-letter-table (i+ (i* 32 idx) n)))

               (num-children ()
                 (declare (type (simple-vector 28) *mask*)
                          (type (simple-vector #.(ash 1 13))
                                *one-bits-per-13*))
                 (let ((first-13 (ilogand node (mask 13)))
                       (second-13 (iash node -13)))
                   (i+ (aref *one-bits-per-13* first-13)
                       (aref *one-bits-per-13* second-13))))

               (trimmed ()
                 (declare (sb-ext:muffle-conditions
                           sb-ext:compiler-note))
                 (subseq word 0 length))
               
               (blank-ps ()
                 (let ((bps (make-array length :initial-element nil)))
                   (loop
                      for idx below num-blanks-active
                      for bs across blank-states
                      for rack-pos = (blank-state-rack-pos bs)
                      do (setf (aref bps rack-pos) t))
                   bps))

               (process-word (start-col)
                 (let* ((word-mul-product 1)
                        (bps (blank-ps))
                        (bingo-p (i= length (rack-capacity rack)))
                        (score
                         (loop
                            with sum = 0
                            with row = center-row
                            for col from start-col
                            for idx below length
                            for tile-idx = (aref word idx)
                            for tile = (aref ,tiles tile-idx)
                            for tile-score = (tile-score tile)
                            for letter-mul = (aref letter-muls row col)
                            for word-mul = (aref word-muls row col)
                            do
                              (setf word-mul-product
                                    (i* word-mul-product word-mul))
                              (unless (aref bps idx)
                                (iincf sum (i* letter-mul tile-score)))
                            finally (return sum)))
                          (score (i* score word-mul-product))
                          (score (if bingo-p
                                     (i+ score bingo-bonus)
                                     score)))
                   (macrolet ((make-this-move ()
                                `(make-move
                                  :action :place
                                  :bingo-p bingo-p
                                  :score score
                                  :direction :horizontal
                                  :start-row center-row
                                  :start-col start-col
                                  :word (trimmed)
                                  :blank-ps bps
                                  :tiles (trimmed)
                                  :already-on-board-ps
                                  (make-array length
                                              :element-type 'boolean
                                              :initial-element nil))))
                     ,(when (eq :TOP-VALUE result-type)
                       `(setf result (imax result score)))
                     ,(when (eq :TOPS result-type)
                       `(let ((top-score (if result
                                             (move-score
                                              (first result))
                                             MOST-NEGATIVE-FIXNUM)))
                          (cond ((i> score top-score)
                                 (setf result (list (make-this-move))))
                                ((i= score top-score)
                                 (push (make-this-move) result)))))
                     ,(when (eq :ALL result-type)
                       `(push (make-this-move) result)))))
                         
               (process-word-in-all-placements ()
                 #+nil
                 ,(when debug-p
                   `(format t "processing word in all placements~%"))
                 (loop
                    with min-col = (i- center-col (i1- length))
                    for start-col from min-col to center-col do
                      (process-word start-col)))

               (blank-hop ()
                 ,(when debug-p
                   `(format t "do the blank-hop!~%"))
                 (setf seq-idx skip-seq-idx
                       elt (aref elts (if (i< seq-idx (length elts))
                                          seq-idx
                                          0)))
                 (loop
                    named :blank-hopping
                    while (iplusp num-blanks-active) do
                      (let* ((next-rack-pos (if (i< seq-idx (length elts))
                                                (perm-elt-change-pos elt)
                                                0))
                             (bs (aref blank-states (i1- num-blanks-active))))
                        ,(when debug-p
                          `(format t "num-blanks-active: ~a~%~
                                      next-rack-pos: ~a~%~
                                      (blank-state-rack-pos bs): ~a~%"
                                   num-blanks-active next-rack-pos
                                   (blank-state-rack-pos bs)))
                        (if (i<= next-rack-pos (blank-state-rack-pos bs))
                            (progn  
                              ,(when debug-p
                                `(format t "moving over blank (~a <= ~a)~%~
                                           (blank-state-idx bs): ~a  ~
                                           (blank-state-limit bs): ~a~%"
                                         next-rack-pos (blank-state-rack-pos bs)
                                         (blank-state-idx bs) (blank-state-limit bs)))
                              (if (i< (blank-state-idx bs) (blank-state-limit bs))
                                  (let ((pe (aref elts (blank-state-seq-idx bs))))
                                    ,(when debug-p
                                      `(format t " advancing blank...~%"))
                                    (iincf (blank-state-idx bs))
                                    (setf seq-idx (blank-state-seq-idx bs)
                                          move-over-blank-p t
                                          rack-pos (blank-state-rack-pos bs)
                                          length (i1+ rack-pos)
                                          dawg-idx (aref rack-pos-dawg-idxs rack-pos)
                                          node (aref nodes dawg-idx)
                                          letter-idx (perm-elt-pos-value pe)
                                          tile-id (aref (rack-tiles rack) letter-idx)
                                          skip-seq-idx (perm-elt-next pe))
                                    (return-from :blank-hopping))
                                  (progn
                                    ,(when debug-p
                                      `(format t " deleting blank...~%"))
                                    (idecf num-blanks-active))))
                            (progn
                              (setf seq-idx skip-seq-idx)
                              (return-from :blank-hopping))))))
               .
               ,(when debug-p
                 `((word-text ()
                     (apply #'concatenate
                            (cons 'string
                                  (loop
                                     for idx below length 
                                     for tile-idx = (aref word idx)
                                     for tile = (and (iplusp tile-idx)
                                                     (aref ,tiles tile-idx))
                                     collect (if tile
                                                 (tile-text tile)
                                                 "_")))))
                   (node-string (bits)
                     (apply #'concatenate
                            (cons 'string
                                  (loop
                                     for letter fixnum below 26
                                     for tile = (aref ,tiles (i1+ letter))
                                     when (logbitp letter bits)
                                     collect (tile-text tile)))))
                   (nth-child-letter (n)
                     ,(when debug-p
                       `(format t "looking for ~ath child-letter at ~a (~a)~%"
                                n dawg-idx (node-string node)))
                     (loop
                        with found = 0
                        for letter below 26 ;; ick!
                        when (node-has-child letter)
                        do (if (i= found n)
                               (return-from
                                nth-child-letter
                                 (i1+ letter))
                               (iincf found)))
                     ,(when debug-p
                       `(format t "did not find ~ath child-letter at ~a (~a)~%"
                                n dawg-idx (node-string node)))))))
        
        (declare (dynamic-extent #'node-has-child
                                 #'fast-child-idx
                                 #'find-child-idx
                                 #'fast-nth-child-letter
                                 #'num-children
                                 #'trimmed
                                 #'blank-ps
                                 #'process-word
                                 #'process-word-in-all-placements
                                 #'blank-hop))
        ,(when debug-p
          `(declare (dynamic-extent #'word-text
                                    #'node-string
                                    #'nth-child-letter)))

        (unless move-over-blank-p
            (setf elt (aref elts seq-idx)
                  rack-pos (perm-elt-change-pos elt)
                  letter-idx (perm-elt-pos-value elt)
                  dawg-idx (aref rack-pos-dawg-idxs rack-pos)
                  node (aref nodes dawg-idx)
                  skip-seq-idx (perm-elt-next elt)
                  tile-id (aref (rack-tiles rack) letter-idx)
                  letter-id +empty+
                  length (i1+ rack-pos)))
          
        ,(when debug-p
          `(format t "====================~%~
                      START of the loop~%~
                      seq-idx: ~a~%~
                      num-blanks-active: ~a~%~
                      move-over-blank-p: ~a~%~
                      rack-pos: ~a~%~
                      ====================~%"
                   seq-idx num-blanks-active move-over-blank-p
                   rack-pos))

        (setf blank-state
              (if (izerop num-blanks-active)
                  nil
                  (aref blank-states (i1- num-blanks-active))))

        ,(when debug-p
               `(format t "seq-idx: ~a~%" seq-idx))
        (when (and (i= tile-id blank-id)
                   (not move-over-blank-p))
          ,(when debug-p
                 `(format t "making new blank-state~%"))
          (setf (aref blank-states num-blanks-active)
                (make-blank-state :idx 0
                                  :limit (i1- (num-children))
                                  :rack-pos rack-pos
                                  :seq-idx seq-idx)
                blank-state (aref blank-states
                                  num-blanks-active))
          (iincf num-blanks-active))

        (setf move-over-blank-p nil)
        
        (if (and blank-state (i= tile-id blank-id))
            (progn
              ,(when debug-p
                `(format t "fast-nth: ~a slow-nth: ~a~%"
                         (fast-nth-child-letter
                              dawg-idx
                              (blank-state-idx blank-state))
                         (nth-child-letter
                              (blank-state-idx blank-state))))
              ,(when debug-p
                `(assert (i= (nth-child-letter
                              (blank-state-idx blank-state))
                             (fast-nth-child-letter
                              dawg-idx
                              (blank-state-idx blank-state)))))
              (setf letter-id (fast-nth-child-letter
                               dawg-idx
                               (blank-state-idx blank-state))
                    #+nil (nth-child-letter (blank-state-idx blank-state))
                    (blank-state-assignment blank-state) letter-id))
            (setf letter-id tile-id))
        
        ,(when debug-p
          `(format t "b-s: ~a~%" blank-state))

        (setf (aref word rack-pos) letter-id)

        ,(when debug-p
          `(format t "letter-id: ~a  tile-id: ~a~%"
                   (tile-name (aref ,tiles letter-id))
                   (tile-name (aref ,tiles tile-id))))

        (let (#+nil (child-idx (find-child-idx (i1- letter-id)))
              (child-idx (fast-child-idx dawg-idx letter-id)))
          ,(when debug-p
            `(let ((slow-child-idx (find-child-idx (i1- letter-id))))
               (when (iplusp child-idx)
                 (assert (equal child-idx slow-child-idx)))))
          ,(when debug-p
            `(format t "child-idx: ~a~%" child-idx))
          (if (iplusp child-idx)
              (let* ((child (aref nodes child-idx))
                     (new-dawg-idx (child-pointer child))
                     (whole-word-p (child-terminates child)))
                (progn
                  ,(when debug-p
                    `(progn
                       (if whole-word-p
                           (format t "~a" (word-text))
                           (format t "~a" (string-downcase
                                             (word-text))))))

                  (when whole-word-p
                    (process-word-in-all-placements))

                  (if (or (izerop new-dawg-idx)
                          (i= rack-pos max-rack-pos))
                      (progn
                        ,(when debug-p
                          `(if (izerop new-dawg-idx)     
                               (format t " has no extensions~%")
                               (format t " is as long as we have ~
                                           letters for~%")))

                        (blank-hop))

                      (progn
                        ,(when debug-p
                          `(format t " has extensions: ~a~%"
                                   (node-string (aref nodes
                                                      new-dawg-idx))))
                        (iincf seq-idx)
                        (setf (aref rack-pos-dawg-idxs
                                    (i1+ rack-pos))
                               new-dawg-idx)))))
              (progn
                ,(when debug-p
                  `(progn
                     (format t "start of new progn at the bottom~%~
                                blank-state: ~a~%"
                             (when (iplusp num-blanks-active)
                               (aref blank-states
                                     (i1- num-blanks-active))))))
                (blank-hop))))))
    result)))

(def-move-finder findloud *standard* *english*
                 *twl* *vanilla* :all t)
(def-move-finder findquiet *standard* *english*
                 *twl* *vanilla* :all nil)
(def-move-finder topsquiet *standard* *english*
                 *twl* *vanilla* :tops nil)
(def-move-finder topvaluequiet *standard* *english*
                 *twl* *vanilla* :top-value nil)

(defmacro o (rack)
  (let ((s (gensym "s")))
    (if (symbolp rack)
      (setf s (symbol-name rack))
      (setf s rack))
    `(findloud nil (new-rack ,s) nil)))

(defmacro oq (rack)
  (let ((s (gensym "s")))
    (if (symbolp rack)
      (setf s (symbol-name rack))
      (setf s rack))
    `(findquiet nil (new-rack ,s) nil)))

(defmacro tq (rack)
  (let ((s (gensym "s")))
    (if (symbolp rack)
      (setf s (symbol-name rack))
      (setf s rack))
    `(topsquiet nil (new-rack ,s) nil)))

(defmacro os (rack)
  `(mapcar (lambda (move) (move-string move *english*)) (o ,rack)))

(defmacro oqs (rack)
  `(mapcar (lambda (move) (move-string move *english*)) (oq ,rack)))

(defmacro tqs (rack)
  `(mapcar (lambda (move) (move-string move *english*)) (tq ,rack)))

(defmacro experimental-def-move-finder (fn-name layout tiles
                                        lexicon rules result-type debug-p)
 `(defun ,fn-name (board unsorted-rack bag)
   ,(if debug-p
       `(declare (optimize debug))
       `(declare (optimize (speed 3) (safety 0) (space 1) (debug 0))))
   (declare (ignore board bag)
            (type (simple-array tile) ,tiles))
   (let* ((sq-infos (board-sq-infos board))
          (rack (rack-sort unsorted-rack))
          (seq (unique-rack-perms rack))
          (elts (perm-seq-elts seq))
          (dawg (lexicon-dawg ,lexicon))
          (nodes (dawg-nodes dawg))
          (nth-child-letter-table (dawg-nth-child-letter-table dawg))
          (letter2child-table (dawg-letter2child-table dawg))
          (rack-pos-dawg-idxs (make-array (i1+ (rack-size rack))
                                          :element-type 'fixnum))
          (max-rack-pos (i1- (rack-size rack)))
          (seq-idx 0)
          (blank-states (make-array (i1+ (rack-size rack))))
          (move-over-blank-p nil)
          (blank-state nil)
          (num-blanks-active 0)
          (blank-id ,(tiles-blank (symbol-value tiles)))
          (blank-tile (aref ,tiles blank-id))
          (blank-quantity (tile-quantity blank-tile))
          (blank-designations (make-array blank-quantity :element-type 'fixnum))
          (word (make-array (the fixnum (rack-size rack)) :element-type 'fixnum))
          (letter-muls ,(layout-letter-muls (symbol-value layout)))
          (word-muls ,(layout-word-muls (symbol-value layout)))
          (center-row ,(layout-center-row (symbol-value layout)))
          (center-col ,(layout-center-col (symbol-value layout)))
          (bingo-bonus ,(rules-bingo-bonus (symbol-value rules)))
          (elt nil)
          (rack-pos nil)
          (letter-idx nil)
          (dawg-idx nil)
          (node 0)
          (skip-seq-idx nil)
          (tile-id nil)
          (letter-id +empty+)
          (length 0)
          (result ,(if (eq result-type :TOP-VALUE)
                       `MOST-NEGATIVE-FIXNUM
                      `nil)))

    (declare (ignore blank-designations)
             (type (simple-array perm-elt) elts)
             (type (simple-array fixnum) nodes)
             ;(type (simple-array fixnum) nth-child-letter-table)
             (type (simple-array (unsigned-byte 8)) nth-child-letter-table)
             (type (simple-array (unsigned-byte 32)) letter2child-table)
             (type fixnum node)
             (type (simple-array fixnum) rack-pos-dawg-idxs))

    ,(when debug-p
      `(format t "rack has ~a permutations~%"
               (length elts)))

    (loop while (i< seq-idx (length elts)) do
      (labels ((node-has-child (child-letter)
                 (logbitp (the fixnum child-letter) node))

               (find-child-idx (child-letter)
                 (when (node-has-child child-letter)
                   (i+ dawg-idx
                       (node-letter-offset node child-letter))))

               (fast-child-idx (idx child-letter)
                 (aref letter2child-table (i+ (i* 32 idx) child-letter)))
               
               (fast-nth-child-letter (idx n)
                 (aref nth-child-letter-table (i+ (i* 32 idx) n)))

               (num-children ()
                 (declare (type (simple-vector 28) *mask*)
                          (type (simple-vector #.(ash 1 13))
                                *one-bits-per-13*))
                 (let ((first-13 (ilogand node (mask 13)))
                       (second-13 (iash node -13)))
                   (i+ (aref *one-bits-per-13* first-13)
                       (aref *one-bits-per-13* second-13))))

               (trimmed ()
                 (declare (sb-ext:muffle-conditions
                           sb-ext:compiler-note))
                 (subseq word 0 length))
               
               (blank-ps ()
                 (let ((bps (make-array length :initial-element nil)))
                   (loop
                      for idx below num-blanks-active
                      for bs across blank-states
                      for rack-pos = (blank-state-rack-pos bs)
                      do (setf (aref bps rack-pos) t))
                   bps))

               (process-word (start-col)
                 (let* ((word-mul-product 1)
                        (bps (blank-ps))
                        (bingo-p (i= length (rack-capacity rack)))
                        (score
                         (loop
                            with sum = 0
                            with row = center-row
                            for col from start-col
                            for idx below length
                            for tile-idx = (aref word idx)
                            for tile = (aref ,tiles tile-idx)
                            for tile-score = (tile-score tile)
                            for letter-mul = (aref letter-muls row col)
                            for word-mul = (aref word-muls row col)
                            do
                              (setf word-mul-product
                                    (i* word-mul-product word-mul))
                              (unless (aref bps idx)
                                (iincf sum (i* letter-mul tile-score)))
                            finally (return sum)))
                          (score (i* score word-mul-product))
                          (score (if bingo-p
                                     (i+ score bingo-bonus)
                                     score)))
                   (macrolet ((make-this-move ()
                                `(make-move
                                  :action :place
                                  :bingo-p bingo-p
                                  :score score
                                  :direction :horizontal
                                  :start-row center-row
                                  :start-col start-col
                                  :word (trimmed)
                                  :blank-ps bps
                                  :tiles (trimmed)
                                  :already-on-board-ps
                                  (make-array length
                                              :element-type 'boolean
                                              :initial-element nil))))
                     ,(when (eq :TOP-VALUE result-type)
                       `(setf result (imax result score)))
                     ,(when (eq :TOPS result-type)
                       `(let ((top-score (if result
                                             (move-score
                                              (first result))
                                             MOST-NEGATIVE-FIXNUM)))
                          (cond ((i> score top-score)
                                 (setf result (list (make-this-move))))
                                ((i= score top-score)
                                 (push (make-this-move) result)))))
                     ,(when (eq :ALL result-type)
                       `(push (make-this-move) result)))))
                         
               (process-word-in-all-placements ()
                 #+nil
                 ,(when debug-p
                   `(format t "processing word in all placements~%"))
                 (loop
                    with min-col = (i- center-col (i1- length))
                    for start-col from min-col to center-col do
                      (process-word start-col)))

               (blank-hop ()
                 ,(when debug-p
                   `(format t "do the blank-hop!~%"))
                 (setf seq-idx skip-seq-idx
                       elt (aref elts (if (i< seq-idx (length elts))
                                          seq-idx
                                          0)))
                 (loop
                    named :blank-hopping
                    while (iplusp num-blanks-active) do
                      (let* ((next-rack-pos (if (i< seq-idx (length elts))
                                                (perm-elt-change-pos elt)
                                                0))
                             (bs (aref blank-states (i1- num-blanks-active))))
                        ,(when debug-p
                          `(format t "num-blanks-active: ~a~%~
                                      next-rack-pos: ~a~%~
                                      (blank-state-rack-pos bs): ~a~%"
                                   num-blanks-active next-rack-pos
                                   (blank-state-rack-pos bs)))
                        (if (i<= next-rack-pos (blank-state-rack-pos bs))
                            (progn  
                              ,(when debug-p
                                `(format t "moving over blank (~a <= ~a)~%~
                                           (blank-state-idx bs): ~a  ~
                                           (blank-state-limit bs): ~a~%"
                                         next-rack-pos (blank-state-rack-pos bs)
                                         (blank-state-idx bs) (blank-state-limit bs)))
                              (if (i< (blank-state-idx bs) (blank-state-limit bs))
                                  (let ((pe (aref elts (blank-state-seq-idx bs))))
                                    ,(when debug-p
                                      `(format t " advancing blank...~%"))
                                    (iincf (blank-state-idx bs))
                                    (setf seq-idx (blank-state-seq-idx bs)
                                          move-over-blank-p t
                                          rack-pos (blank-state-rack-pos bs)
                                          length (i1+ rack-pos)
                                          dawg-idx (aref rack-pos-dawg-idxs rack-pos)
                                          node (aref nodes dawg-idx)
                                          letter-idx (perm-elt-pos-value pe)
                                          tile-id (aref (rack-tiles rack) letter-idx)
                                          skip-seq-idx (perm-elt-next pe))
                                    (return-from :blank-hopping))
                                  (progn
                                    ,(when debug-p
                                      `(format t " deleting blank...~%"))
                                    (idecf num-blanks-active))))
                            (progn
                              (setf seq-idx skip-seq-idx)
                              (return-from :blank-hopping))))))
               .
               ,(when debug-p
                 `((word-text ()
                     (apply #'concatenate
                            (cons 'string
                                  (loop
                                     for idx below length 
                                     for tile-idx = (aref word idx)
                                     for tile = (and (iplusp tile-idx)
                                                     (aref ,tiles tile-idx))
                                     collect (if tile
                                                 (tile-text tile)
                                                 "_")))))
                  (node-string (bits)
                    (apply #'concatenate
                           (cons 'string
                                 (loop
                                    for letter fixnum below 26
                                    for tile = (aref ,tiles (i1+ letter))
                                    when (logbitp letter bits)
                                    collect (tile-text tile)))))
                   (nth-child-letter (n)
                     ,(when debug-p
                       `(format t "looking for ~ath child-letter at ~a (~a)~%"
                                n dawg-idx (node-string node)))
                     (loop
                        with found = 0
                        for letter below 26 ;; ick!
                        when (node-has-child letter)
                        do (if (i= found n)
                               (return-from
                                nth-child-letter
                                 (i1+ letter))
                               (iincf found)))
                     ,(when debug-p
                       `(format t "did not find ~ath child-letter at ~a (~a)~%"
                                n dawg-idx (node-string node)))))))
        
        (declare (dynamic-extent #'node-has-child
                                 #'fast-child-idx
                                 #'find-child-idx
                                 #'fast-nth-child-letter
                                 #'num-children
                                 #'trimmed
                                 #'blank-ps
                                 #'process-word
                                 #'process-word-in-all-placements
                                 #'blank-hop))
        ,(when debug-p
          `(declare (dynamic-extent #'word-text
                                    #'node-string
                                    #'nth-child-letter)))

        (unless move-over-blank-p
            (setf elt (aref elts seq-idx)
                  rack-pos (perm-elt-change-pos elt)
                  letter-idx (perm-elt-pos-value elt)
                  dawg-idx (aref rack-pos-dawg-idxs rack-pos)
                  node (aref nodes dawg-idx)
                  skip-seq-idx (perm-elt-next elt)
                  tile-id (aref (rack-tiles rack) letter-idx)
                  letter-id +empty+
                  length (i1+ rack-pos)))
          
        ,(when debug-p
          `(format t "====================~%~
                      START of the loop~%~
                      seq-idx: ~a~%~
                      num-blanks-active: ~a~%~
                      move-over-blank-p: ~a~%~
                      rack-pos: ~a~%~
                      ====================~%"
                   seq-idx num-blanks-active move-over-blank-p
                   rack-pos))

        (setf blank-state
              (if (izerop num-blanks-active)
                  nil
                  (aref blank-states (i1- num-blanks-active))))

        ,(when debug-p
               `(format t "seq-idx: ~a~%" seq-idx))
        (when (and (i= tile-id blank-id)
                   (not move-over-blank-p))
          ,(when debug-p
                 `(format t "making new blank-state~%"))
          (setf (aref blank-states num-blanks-active)
                (make-blank-state :idx 0
                                  :limit (i1- (num-children))
                                  :rack-pos rack-pos
                                  :seq-idx seq-idx)
                blank-state (aref blank-states
                                  num-blanks-active))
          (iincf num-blanks-active))

        (setf move-over-blank-p nil)
        
        (if (and blank-state (i= tile-id blank-id))
            (progn
              ,(when debug-p
                `(format t "fast-nth: ~a slow-nth: ~a~%"
                         (fast-nth-child-letter
                              dawg-idx
                              (blank-state-idx blank-state))
                         (nth-child-letter
                              (blank-state-idx blank-state))))
              ,(when debug-p
                `(assert (i= (nth-child-letter
                              (blank-state-idx blank-state))
                             (fast-nth-child-letter
                              dawg-idx
                              (blank-state-idx blank-state)))))
              (setf letter-id (fast-nth-child-letter
                               dawg-idx
                               (blank-state-idx blank-state))
                    #+nil (nth-child-letter (blank-state-idx blank-state))
                    (blank-state-assignment blank-state) letter-id))
            (setf letter-id tile-id))
        
        ,(when debug-p
          `(format t "b-s: ~a~%" blank-state))

        (setf (aref word rack-pos) letter-id)

        ,(when debug-p
          `(format t "letter-id: ~a  tile-id: ~a~%"
                   (tile-name (aref ,tiles letter-id))
                   (tile-name (aref ,tiles tile-id))))

        (let (#+nil (child-idx (find-child-idx (i1- letter-id)))
              (child-idx (fast-child-idx dawg-idx letter-id)))
          ,(when debug-p
            `(let ((slow-child-idx (find-child-idx (i1- letter-id))))
               (when (iplusp child-idx)
                 (assert (equal child-idx slow-child-idx)))))
          ,(when debug-p
            `(format t "child-idx: ~a~%" child-idx))
          (if (iplusp child-idx)
              (let* ((child (aref nodes child-idx))
                     (new-dawg-idx (child-pointer child))
                     (whole-word-p (child-terminates child)))
                (progn
                  ,(when debug-p
                    `(progn
                       (if whole-word-p
                           (format t "~a" (word-text))
                           (format t "~a" (string-downcase
                                             (word-text))))))

                  (when whole-word-p
                    (process-word-in-all-placements))

                  (if (or (izerop new-dawg-idx)
                          (i= rack-pos max-rack-pos))
                      (progn
                        ,(when debug-p
                          `(if (izerop new-dawg-idx)     
                               (format t " has no extensions~%")
                               (format t " is as long as we have ~
                                           letters for~%")))

                        (blank-hop))

                      (progn
                        ,(when debug-p
                          `(format t " has extensions: ~a~%"
                                   (node-string (aref nodes
                                                      new-dawg-idx))))
                        (iincf seq-idx)
                        (setf (aref rack-pos-dawg-idxs
                                    (i1+ rack-pos))
                               new-dawg-idx)))))
              (progn
                ,(when debug-p
                  `(progn
                     (format t "start of new progn at the bottom~%~
                                blank-state: ~a~%"
                             (when (iplusp num-blanks-active)
                               (aref blank-states
                                     (i1- num-blanks-active))))))
                (blank-hop))))))
    result)))
