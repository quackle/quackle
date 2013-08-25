(in-package :ouat)

(defstruct layout
  height
  width
  center-row
  center-col
  letter-muls
  word-muls)

(defun new-symmetric-square-layout (eighth)
  (let* ((center (1- (length eighth)))
         (size (+ 1 center center))
         (letter-muls (make-array `(,size ,size) :initial-element 1))
         (word-muls (make-array `(,size ,size) :initial-element 1)))
    (loop
       for row-list in eighth
       for row from 0
       do (loop
             with squares = (string-tokens row-list)
             for square in squares
             for col from 0
             for mul = (parse-integer square :junk-allowed t)
             for array = (case (elt square 1)
                           (#\L letter-muls)
                           (#\W word-muls))
             when (and mul array)
             do (setf (aref array row col) mul
                      (aref array col row) mul
                      (aref array (- size row 1) col) mul
                      (aref array row (- size col 1)) mul
                      (aref array (- size row 1) (- size col 1)) mul
                      (aref array (- size col 1) (- size row 1)) mul
                      (aref array (- size col 1) row) mul
                      (aref array col (- size row 1)) mul)))
    (make-layout :height size
                 :width size
                 :center-row center
                 :center-col center
                 :letter-muls letter-muls
                 :word-muls word-muls)))

(defstruct tile
  text
  blank-text
  quantity
  score
  type
  name)

(defun text-tile (text tiles)
  (loop
     for index fixnum from 1
     for tile across (subseq tiles 1) do
       (when (string= text (tile-text tile))
	 (return-from text-tile index))))

(defun tiles-blank (tiles)
  (loop
     for index fixnum from 1
     for tile across (subseq tiles 1) do
       (when (eq :BLANK (tile-type tile))
	 (return-from tiles-blank index))))
       
(defstruct board
  layout
  (empty-p t)
  blank-ps
  letters
  sq-infos
  nodes)

(defstruct sq-info
  h
  v)

(defstruct h/v
  cross
  score
  nodes
  scores
  positions)

(defun new-sq-info ()
  (make-sq-info :h (make-h/v) :v (make-h/v)))
     
(defun new-board (layout)
  (labels ((new-board-array (type initial-element)
             (make-array (list (layout-height layout)
                               (layout-width layout))
                          :element-type type 
                          :initial-element initial-element)))
    (let ((board
           (make-board :layout layout
                       :blank-ps (new-board-array 'boolean nil)
                       :letters (new-board-array 'fixnum +empty+)
                       :sq-infos (new-board-array t nil))))
      (dotimes (row (layout-height layout))
        (dotimes (col (layout-height layout))
          (setf (aref (board-sq-infos board) row col) (new-sq-info))))
      board)))

(defun mul-text (letter-mul word-mul letter-mul-texts word-mul-texts)
  (if (i< 1 letter-mul)
      (aref letter-mul-texts letter-mul)
      (aref word-mul-texts word-mul)))

(defun board-row-pretty-squares (board row tiles
                                 letter-mul-texts word-mul-texts)
  (loop
     with layout = (board-layout board)
     for col below (layout-width layout)
     for letter = (aref (board-letters board) row col)
     collect
       (if (iplusp letter)
           (let ((letter-tile (aref tiles letter)))
             (if (aref (board-blank-ps board) row col)
                 (tile-blank-text letter-tile)
                 (tile-text letter-tile)))
           (mul-text (aref (layout-letter-muls layout) row col)
                     (aref (layout-word-muls layout) row col)
                     letter-mul-texts word-mul-texts))))

(defun board-pretty-squares (board tiles letter-mul-texts
                             word-mul-texts)
  (loop
     with layout = (board-layout board)
     for row below (layout-height layout) collect
       (board-row-pretty-squares board row tiles letter-mul-texts
                                 word-mul-texts)))

(defun place-move (board move)
  (when (eq (move-action move) :place)
    (setf (board-empty-p board) nil)
    (loop
       with row = (move-start-row move)
       with col = (move-start-col move)
       for letter across (move-word move)
       for blank-p across (move-blank-ps move)
       for already-p across (move-already-on-board-ps move)
       unless already-p do
         (setf (aref (board-letters board) row col) letter
               (aref (board-blank-ps board) row col) blank-p)
       do (if (eq (move-direction move) :horizontal)
              (iincf col)
              (iincf row)))))

(defstruct rack
  capacity
  (tiles (make-array '(30) :element-type 'fixnum)
         :type (simple-array fixnum)))

(defun rack-texts (rack tiles)
  (loop
     for tile being the elements of (rack-tiles rack) collect
       (tile-text (aref tiles tile))))

(defun rack-sort (rack)
  (make-rack :capacity (rack-capacity rack)
             :tiles (sort (copy-seq (rack-tiles rack)) '<)))

(defun rack-size (rack)
  (loop
     with num-tiles = 0
     for tile across (rack-tiles rack)
     when (iplusp tile)
     do (iincf num-tiles)
     finally (return (the fixnum num-tiles))))

(defstruct move
  action ; (:place :exchange :pass :deadwood :time-penalty)
  challenged-phony-p
  bingo-p
  score
  static-equity
  simmed-equity
  win%
  
  direction ; (:horizontal :vertical)
  start-row
  start-col
  word
  blank-ps
  tiles
  already-on-board-ps)

(defun move-string (move tiles)
  (when (eq (move-action move) :place)
    (let* ((row (row2uv-row (move-start-row move)))
           (col (col2uv-col (move-start-col move)))
           (pos (if (eq :vertical (move-direction move))
                    (format nil "~a~a" col row)
                    (format nil "~a~a" row col)))
           (word (apply #'concatenate 
                        (cons
                         'string
                         (loop
                            for tile-id across (move-word move)
                            for tile = (aref tiles tile-id)  
                            for blank-p across (move-blank-ps move)
                            collect (if blank-p
                                        (tile-blank-text tile)
                                        (tile-text tile)))))))
      (format nil "~a ~a" pos word))))
    
(defstruct dawg
  nodes
  nth-child-letter-table
  letter2child-table)

(defstruct filldawg
  nodes)

(defun new-dawg (nodes)
  (let ((nth-child-table (make-array (* 32 (length nodes))
                                     :element-type '(unsigned-byte 8)))
        (letter2child-table (make-array (* 32 (length nodes))
                                        :element-type '(unsigned-byte 32))))
    (loop
       for node-idx from 0
       for node across nodes do
         (labels ((node-has-child (child-letter)
                    (logbitp child-letter node)))
           (loop
              with n = 0
              for letter below 26
              when (node-has-child letter) do
                (setf (aref letter2child-table (i+ (i* 32 node-idx) (i1+ letter)))
                      (i+ node-idx (node-letter-offset node letter)))
                (setf (aref nth-child-table (i+ (i* 32 node-idx) n)) (i1+ letter))
                (iincf n))))
    (make-dawg :nodes nodes
               :nth-child-letter-table nth-child-table
               :letter2child-table letter2child-table)))

(defstruct gaddag
  nodes)

(defstruct lexicon
  dawg
  filldawg)

(defstruct filldawg-node
  (data 0)
  (children nil))

(defun filldawg-node-letter (node)
  (logand (filldawg-node-data node) #.(i1- (iash 1 5))))

(defun set-filldawg-node-letter (node letter)
  (assert (typep letter '(integer 0 #.(i1- (iash 1 5)))))
  (setf (filldawg-node-data node)
        (logior (logand (filldawg-node-data node)
                          #.(ilognot (i1- (iash 1 5))))
                letter)))

(defun filldawg-node-terminal-p (node)
  (logbitp 5 (filldawg-node-data node)))

(defun set-filldawg-node-terminal-p (node terminal-p)
  (setf (logbitp 5 (filldawg-node-data node)) terminal-p))

(defun filldawg-node-idx (node)
  (ash (filldawg-node-data node) -6))

(defun set-filldawg-node-idx (node idx)
  (setf (filldawg-node-data node)
        (logior (logand (filldawg-node-data node)
                          #.(i1- (iash 1 6)))
                 (ash idx 6))))

(defun filldawg-node-child-which-has-letter (node letter)
  (find-if #'(lambda (child)
               (i= letter (filldawg-node-letter child)))
           (filldawg-node-children node)))

(defun filldawg-node-child-which-has-letter! (node letter)
  (or (filldawg-node-child-which-has-letter node letter)
      (first (push (make-filldawg-node :data letter)
                   (filldawg-node-children node)))))

(defun filldawg-push-suffix (suffix node)
  (set-filldawg-node-idx node 0)
  (let* ((letter (first suffix))
         (new-suffix (rest suffix)))
    (if suffix
        (filldawg-push-suffix
         new-suffix
         (filldawg-node-child-which-has-letter! node letter))
        (set-filldawg-node-terminal-p node t))))

(defun filldawg-push-word (word trie)
  (filldawg-push-suffix (coerce word 'list) trie))

(defun filldawg-sort-children (node)
  (setf (filldawg-node-children node)
        (sort (filldawg-node-children node)
              '< :key 'filldawg-node-letter))
  (dolist (child (filldawg-node-children node))
    (filldawg-sort-children child)))

(defun filldawg-node-child-bits (node)
  (let ((child-bits 0))
    (dolist (child (filldawg-node-children node))
      (setf (logbitp (filldawg-node-letter child) child-bits) t))
    child-bits))

(defun filldawg-node-bits (node)
  (let ((bits (filldawg-node-idx node)))
    (if (filldawg-node-terminal-p node)
        (logior bits (ash 1 28))
        bits)))

(defun filldawg-node-whole-word-p (node)
  (logbitp 28 node))

(defun filldawg-number-trie (node)
  (let ((index 0))
    (labels ((number-subtrie (node)
               (if (filldawg-node-children node)
                   (progn
                     (set-filldawg-node-idx node index)
                     (incf index)
                     (incf index (length (filldawg-node-children node)))
                     (dolist (child (filldawg-node-children node))
                       (number-subtrie child)))
                   (set-filldawg-node-idx node 0))))
      (number-subtrie node))
    index))

(defun suffix-completes-a-word-p (suffix dawg-idx lexicon)
  (declare (optimize (safety 3) (debug 3)))
  (destructuring-bind (letter . new-suffix)
      suffix
    (let* ((dawg (lexicon-dawg lexicon))
           (nodes (dawg-nodes dawg))
           (letter2child-table (dawg-letter2child-table dawg)))
      (labels ((fast-child-idx (child-letter)
                 (aref letter2child-table
                       (i+ (i* 32 dawg-idx) child-letter))))
        (let ((child-idx (fast-child-idx (i1+ letter))))
          (when (iplusp child-idx)
            (let* ((child (aref nodes child-idx))
                   (new-dawg-idx (child-pointer child))
                   (whole-word-p (child-terminates child)))
              (if new-suffix
                  (and (iplusp new-dawg-idx)
                       (suffix-completes-a-word-p
                        new-suffix new-dawg-idx lexicon))
                  whole-word-p))))))))

;; Stupid and temporary anglocentric hack.
(defconstant +thru-delimiter+ 26)
(defconstant +segment-delimiter+ 27)

(defun filldawg-node-children-string (filldawg-idx lexicon tiles)
  (let* ((filldawg (lexicon-filldawg lexicon))
         (nodes (filldawg-nodes filldawg))
         (node (aref nodes filldawg-idx)))
    (apply #'concatenate
           (cons 'string
                 (append
                  (loop
                     for letter fixnum below 26
                     for tile = (aref tiles (i1+ letter))
                     when (logbitp letter node)
                     collect (tile-text tile))
                  (if (logbitp #.+thru-delimiter+ node) '("-") nil)
                  (if (logbitp #.+segment-delimiter+ node) '("/") nil))))))

(defun filldawg-prefix-node-idx (prefix-suffix filldawg-idx lexicon)
  (when (null prefix-suffix)
    (return-from filldawg-prefix-node-idx filldawg-idx))
  #+nil
  (format t "prefix-suffix: ~a filldawg-idx: ~a~%"
          (pattern2string prefix-suffix) filldawg-idx)
  (let* ((filldawg (lexicon-filldawg lexicon))
         (nodes (filldawg-nodes filldawg))
         (node (aref nodes filldawg-idx)))
    (labels ((node-has-child (datum)
               (logbitp datum node))
             (node-datum-offset (datum)
               (let ((mask (1- (iash 1 datum))))
                 (1+ (logcount (logand node mask)))))
             (find-child-idx (datum)
               (when (node-has-child datum)
                 (i+ filldawg-idx
                     (node-datum-offset datum)))))
      (destructuring-bind (datum . new-prefix-suffix)
          prefix-suffix
        #+nil
        (format t "datum: ~a filldawg-idx: ~a node: ~a~%"
                datum filldawg-idx node)
        (let ((child-idx (find-child-idx datum)))
          (when child-idx
            (let* ((child (aref nodes child-idx))
                   (new-filldawg-idx (child-pointer child)))
              (filldawg-prefix-node-idx new-prefix-suffix
                                        new-filldawg-idx
                                        lexicon))))))))

(defun filldawg-string-extensions (string lexicon)
  (let ((prefix (filldawg-string2list string)))
    (mapcar #'pattern2string
            (filldawg-extensions prefix
                                 (filldawg-prefix-node-idx prefix
                                                           0 *twl*)
                                 lexicon))))

(defun filldawg-extensions (prefix filldawg-idx lexicon)
  (declare (optimize debug))
  (when (null filldawg-idx)
    (return-from filldawg-extensions nil))
  #+nil
  (format t "prefix: ~a  filldawg-idx:~a~%"
          (pattern2string prefix) filldawg-idx)
  (let* ((filldawg (lexicon-filldawg lexicon))
         (nodes (filldawg-nodes filldawg))
         (node (aref nodes filldawg-idx)))
    (labels ((node-has-child (datum)
               (logbitp datum node))
             (num-children ()
               (logcount node))
             (nth-child-datum (n)
               (loop
                  with found = 0
                  for datum below 28
                  when (node-has-child datum)
                  do (if (i= found n)
                         (return-from nth-child-datum datum)
                         (iincf found)))))
      (apply #'append
              (loop
                 for n below (num-children)
                 for datum = (nth-child-datum n)
                 for child-idx = (+ 1 n filldawg-idx)
                 for child-node = (aref nodes child-idx)
                 for new-filldawg-idx = (logand child-node
                                                (1- (ash 1 28)))
                 for new-prefix = (append prefix (list datum))
                 when (filldawg-node-whole-word-p child-node)
                 collect (list new-prefix)
                 when (iplusp new-filldawg-idx)
                 collect (filldawg-extensions new-prefix
                                              new-filldawg-idx
                                              lexicon))))))

(defun list-is-word-p (word &optional (lexicon *twl*))
  (suffix-completes-a-word-p word 0 lexicon))

(defun filldawg-patterns (word max-new-tiles max-thrus allow-phony-thrus-p)
  ;(declare (optimize (speed 3) (safety 0) (space 1) (debug 0)))
  (let ((possible-splits nil))
    (labels ((push-partitions (prefix-groups suffix)
               (push (append prefix-groups (list suffix)) possible-splits)
               (loop 
                  for first-length from 1 to (i1- (length suffix))
                  for first = (subseq (the list suffix) 0 first-length)
                  for rest = (subseq (the list suffix) first-length)
                  do (push-partitions (append prefix-groups (list first)) rest)))
             (all-valid (thru-strings)
               (when (i> (length thru-strings) max-thrus)
                 (return-from all-valid nil))
               (let* ((num-thru-tiles (apply '+ (mapcar #'length thru-strings)))
                      (num-gap-tiles (i- (length word) num-thru-tiles)))
                 (when (or (i> num-gap-tiles max-new-tiles)
                           (izerop num-gap-tiles))
                   (return-from all-valid nil))
                 (when allow-phony-thrus-p
                   (return-from all-valid t))
                 (loop 
                    for string in thru-strings
                    always (or (null (cdr string))
                               (list-is-word-p string)))))
             (collect-thru/gap-starting (split thru-p)
               (loop
                  for thru in split
                  for idx fixnum from 0
                  when (i= (ilogand idx 1) (if thru-p 0 1))
                  collect thru))
             (codify-split (thrus gaps)
               (append (apply #'append
                              (loop
                                 for idx from 0
                                 for thru in thrus
                                 collect (append thru
                                                 (if (i< idx (i1- (length thrus)))
                                                     (list #.+thru-delimiter+)
                                                     nil))))
                       (list #.+segment-delimiter+)
                       (loop
                          for gap in (if (i< (length thrus) (length gaps))
                                         (butlast gaps)
                                         gaps)
                          collect (length gap))
                       (list #.+segment-delimiter+)
                       (apply #'append gaps))))
      (push-partitions nil word)
      (loop
         for split in possible-splits
         for thru-starting-thrus = (collect-thru/gap-starting split t)
         for gap-starting-thrus = (collect-thru/gap-starting split nil)
         when (all-valid thru-starting-thrus)
         collect (codify-split thru-starting-thrus
                               (cons nil gap-starting-thrus))
         when (all-valid gap-starting-thrus)
         collect (codify-split gap-starting-thrus thru-starting-thrus)))))

(defun pattern2string (pattern)
  (coerce (loop
             with segment = 0
             for datum in pattern collect
               (cond ((= datum +thru-delimiter+)
                      #\-)
                     ((= datum +segment-delimiter+)
                      (progn (incf segment) #\/))
                     ((= segment 1) ;; gap lengths
                      (code-char (+ (char-code #\0) datum)))
                     (t             ;; thrus or gaps
                      (letter2char datum))))
          'string))

(defun filldawg-string2list (string)
  (loop 
     for letter across string
     collect (cond ((and (>= (char-code letter) (char-code #\A))
                         (<= (char-code letter) (char-code #\Z))) 
                    (- (char-code letter) (char-code #\A)))
                   ((and (>= (char-code letter) (char-code #\a))
                         (<= (char-code letter) (char-code #\z))) 
                    (- (char-code letter) (char-code #\a)))
                   ((eq letter #\-) +thru-delimiter+)
                   ((eq letter #\/) +segment-delimiter+)
                   (t (- (char-code letter) (char-code #\0))))))

(defun filldawg-trie-from-file (&optional filename (max-newly-placed 7)
                                          (max-thrus MOST-POSITIVE-FIXNUM))
  (setf filename (string-append *data-directory* (or filename "twl.txt")))
  (let ((trie (make-filldawg-node)))
    (do-file-lines (word filename)
      (let* ((word-list (word2list word))
             (patterns (filldawg-patterns word-list
                                          max-newly-placed
                                          max-thrus
                                          nil)))
        (dolist (pattern patterns)
          (filldawg-push-word pattern trie))))
    trie))

(defun count-filldawg-patterns (&optional filename (max-newly-placed 7)
                                          (max-thrus MOST-POSITIVE-FIXNUM))
                                
  (setf filename (string-append *data-directory* (or filename "twl.txt")))
  (let ((num-patterns 0))
    (do-file-lines (word filename)
      (let* ((word-list (word2list word))
             (patterns (filldawg-patterns word-list
                                          max-newly-placed
                                          max-thrus
                                          t)))
        (incf num-patterns (length patterns))))
    num-patterns))

(defun filldawg-from-wordlist (wordlist filldawg
                               &optional (max-newly-placed 7)
                               (max-thrus MOST-POSITIVE-FIXNUM))
   (let* ((trie (filldawg-trie-from-file wordlist
                                         max-newly-placed
                                         max-thrus))
          (filldawg (string-append *data-directory* filldawg))
          (size (progn
                  (filldawg-sort-children trie)
                  (filldawg-number-trie trie))))
    (with-open-file (stream
                     filldawg
                     :direction :output
                     :element-type '(unsigned-byte 32)
                     :if-does-not-exist :create)
      (labels ((write-node (node)
                 (when (filldawg-node-children node)
                   (write-byte (filldawg-node-child-bits node) stream)
                   (dolist (child (filldawg-node-children node))
                     (write-byte (filldawg-node-bits child) stream))
                   (dolist (child (filldawg-node-children node))
                     (write-node child)))))
        (write-byte size stream)
        (write-node trie)))))

;; http://en.wikipedia.org/wiki/Scrabble_variants
(defstruct rules
  clabbers-p ; implemented in the dawg
  woadwage-p ; lexicon = words + concatenations of two words
  escalating-p ; bingo player's rack grows by one
  recycle-p ; swap on-board blank with on-rack blank representation
  if-only-p ; may turn one natural tile into a blank
  toroidal-p 
  bingo-bonus  
  max-scoreless-turns
  challenge-rule ; (:single :double :penalty-per-word :penalty-per-move)
  challenge-penalty
  overdraw-rule ; (:naspa :wespa)
  time-limit
  time-penalty-duration
  time-penalty)

(defun list2text (list &optional (tiles *english*))
  (apply #'concatenate
          (cons 'string
                (mapcar (lambda (letter)
                          (tile-text (aref tiles letter)))
                        list))))

(defun fit-between (before after lexicon)
  (declare (optimize (debug 3)))
  (unless (or before after)
    (return-from fit-between +all-letters+))
  (let* ((pattern
          (cond ((null before) (append after '(#.+segment-delimiter+ 1
                                               #.+segment-delimiter+)))
                ((null after) (append before '(#.+segment-delimiter+ 0
                                               #.+segment-delimiter+)))
                (t (append before '(#.+thru-delimiter+) after
                           '(#.+segment-delimiter+ 0 1
                             #.+segment-delimiter+)))))
         (filldawg (lexicon-filldawg lexicon))
         (nodes (filldawg-nodes filldawg))
         (node-idx (filldawg-prefix-node-idx pattern 0 lexicon))
         (node (and node-idx (aref nodes node-idx))))
    (when (null node)
      (return-from fit-between 0))
    (labels ((node-has-child (letter)
               (logbitp letter node))
             (num-children ()
               (logcount node))
             (nth-child-letter (n)
               (loop
                  with found = 0
                  for letter below 26
                  when (node-has-child letter)
                  do (if (i= found n)
                         (return-from nth-child-letter letter)
                         (iincf found)))))
      (let ((bits 0))
        (loop
           for n below (num-children)
           for letter = (nth-child-letter n)
           for child-idx = (+ 1 n node-idx)
           for child-node = (aref nodes child-idx)
           when (filldawg-node-whole-word-p child-node) do
             (setf (logbitp letter bits) t))
        bits))))

(defun zero-aligned-bit-letters (bits tiles)
  (apply #'concatenate
         (cons 'string
               (loop
                  for letter fixnum below 26
                  for tile = (aref tiles (1+ letter))
                  when (logbitp letter bits)
                  collect (tile-text tile)))))

(defun string-fit-between (before after
                           &optional (lexicon *twl*) (tiles *english*))
  (zero-aligned-bit-letters (fit-between (word2list before)
                                         (word2list after)
                                         lexicon)
                            tiles))
                       
(defun update-sq-infos (board lexicon tiles
                        &optional (debug-p t) (rack-size 7) last-move)
 (declare (optimize (debug 3)))
 (declare (ignore last-move))
 (let* ((layout (board-layout board))
        (height (layout-height layout))
        (width (layout-width layout))
        (letters (board-letters board))
        (blank-ps (board-blank-ps board))
        (sq-infos (board-sq-infos board))
        (nodes (board-nodes board))
        (blank (tiles-blank tiles)))
  (macrolet ((do-squares ((row col dir sq-info h/v) &body body)
               `(dotimes (,row height)
                  (dotimes (,col width)
                    (dolist (,dir '(:horizontal :vertical))
                      (let* ((,sq-info (aref sq-infos row col))
                             (,h/v (if (eq ,dir :horizontal)
                                       (sq-info-v ,sq-info)
                                       (sq-info-h ,sq-info))))
                        ,@body))))))
   (labels ((on-board-p (r c)
               (and (<= 0 r (1- height))
                    (<= 0 c (1- width))))
             (next-sq (r c l-to-r-p dir)
               (if (eq dir :horizontal)
                   (if l-to-r-p
                       (values r (i1+ c))
                       (values r (i1- c)))
                   (if l-to-r-p
                       (values (i1+ r) c)
                       (values (i1- r) c))))
             (get-word (r c l-to-r-p dir)
               (multiple-value-bind (next-r next-c)
                   (next-sq r c l-to-r-p dir)
                 (when (on-board-p next-r next-c)
                   (let ((letter (aref letters next-r next-c)))
                     (unless (i= letter +empty+)
                       (append (list (1- (aref letters next-r next-c)))
                               (get-word next-r next-c l-to-r-p dir)))))))
             (get-score (r c l-to-r-p dir)
               (multiple-value-bind (next-r next-c)
                   (next-sq r c l-to-r-p dir)
                 (if (on-board-p next-r next-c)
                     (let ((letter (aref letters next-r next-c)))
                       (if (i= letter +empty+)
                           0
                           (let* ((blank-p (aref blank-ps next-r next-c))
                                  (tile-idx (if blank-p blank letter))
                                  (tile (aref tiles tile-idx))
                                  (score (tile-score tile)))
                             (i+ score
                                 (get-score next-r next-c l-to-r-p dir)))))
                     0)))
             (thru-words (row col dir)
               #+nil
               (format t  "(thru-words ~a ~a) dir: ~a~%"
                       row col dir)
               (multiple-value-bind (prev-r prev-c)
                   (next-sq row col nil dir)
                 (when (and (on-board-p prev-r prev-c)
                            (iplusp (aref letters prev-r prev-c)))
                   (return-from thru-words nil)))
               (let ((words nil)
                     (gaps (list 0))
                     (last-tile :gap)
                     (num-tiles 0)
                     (r row)
                     (c col))
                 (loop
                    (unless (on-board-p r c)
                      (return-from thru-words
                        (values (reverse (mapcar #'reverse words))
                                (if (eq last-tile :gap)
                                    (reverse (rest gaps))
                                    (reverse gaps)))))
                    (let ((letter (aref letters r c)))
                      #+nil
                      (format t "r: ~a c: ~a letter: ~a words: ~a gaps: ~a ~
                                  last: ~a num-tiles: ~a~%"
                              r c letter words gaps last-tile num-tiles)
                      (if (izerop letter)
                          (progn
                            (if (eq last-tile :gap)
                                (iincf (first gaps))
                                (progn
                                  (push 1 gaps)
                                  (setf last-tile :gap)))
                            (incf num-tiles)
                            (when (i> num-tiles rack-size)
                              (return-from thru-words
                                (values (reverse (mapcar #'reverse words))
                                        (reverse (rest gaps))))))
                          (if (eq last-tile :gap)
                              (progn
                                (push (list letter) words)
                                (setf last-tile :letter))
                              (push letter (first words))))
                      (multiple-value-setq (r c)
                        (next-sq r c t dir))))))
             (pattern (thrus gaps)
               (when (null thrus) (return-from pattern nil))
               (apply #'append
                      (loop
                         for (thru . more) on thrus
                         collect (mapcar #'1- thru)
                         when more
                         collect (list +thru-delimiter+)
                         unless more
                         collect (append (list +segment-delimiter+)
                                         gaps
                                         (list +segment-delimiter+)))))
             (cross-at (r c dir)
               (let* ((sq-info (aref sq-infos r c))
                      (h/v (if (eq dir :horizontal)
                               (sq-info-h sq-info)
                               (sq-info-v sq-info))))
                 (h/v-cross h/v)))
             (score-at (r c)
               (let ((letter (aref letters r c)))
                 (if (i= letter +empty+)
                     0
                     (let* ((blank-p (aref blank-ps r c))
                            (tile-idx (if blank-p blank letter))
                            (tile (aref tiles tile-idx)))
                       (tile-score tile)))))
             (patterns (thrus gaps row col dir)
               (let ((patterns (make-array `(,rack-size)))
                     (scores (make-array `(,rack-size)))
                     (positions (make-array `(,rack-size))))
                 (loop
                    ;; Really need to clean this up
                    with min-tiles = 0
                    with score = 0
                    with r = row
                    with c = col
                    for num-used from 0 to (length thrus)
                    for used-thrus = (subseq thrus 0 num-used)
                    for used-gaps = (subseq gaps 0 num-used)
                    for next-thru-size = (if thrus
                                             (length (first (last used-thrus)))
                                             0)
                    for next-gap = (if (i= num-used (length thrus))
                                       rack-size
                                       (nth num-used gaps))
                    for max-tiles = (if (i< num-used (length thrus))
                                        (if used-thrus
                                            (if (and (izerop (first gaps))
                                                     (i= num-used 1))
                                                (i+ min-tiles next-gap -2)
                                                (i+ min-tiles next-gap -1))
                                            (i+ min-tiles next-gap -2))
                                        (i1- rack-size))
                    for ahead-r = r
                    for ahead-c = c
                    do
                      (loop
                         named :find-nonempty-square do
                           (when (or (not (on-board-p ahead-r ahead-c))
                                     (iplusp (aref letters ahead-r ahead-c)))
                             (return-from :find-nonempty-square))
                           (multiple-value-setq (ahead-r ahead-c)
                             (next-sq ahead-r ahead-c t dir)))
                      (loop
                         repeat next-thru-size do
                           (when debug-p
                             (format t "ahead-r: ~a ahead-c: ~a~%" ahead-r ahead-c))
                           (when (on-board-p ahead-r ahead-c)
                             (when debug-p
                               (format t "(score-at ahead-r ahead-c): ~a~%"
                                       (score-at ahead-r ahead-c)))
                             (iincf score (score-at ahead-r ahead-c)))
                           (multiple-value-setq (ahead-r ahead-c)
                             (next-sq ahead-r ahead-c t dir)))
                      (when debug-p
                        (format t "min-tiles: ~a num-used: ~a max-tiles: ~a ~
                                   used-thrus: ~a~%~
                                   used-gaps: ~a next-gap: ~a next-thru-size: ~a~%"
                                min-tiles num-used max-tiles used-thrus used-gaps
                                next-gap next-thru-size))
                      (when (i<= 0 max-tiles)
                        (loop
                           with hooked-p = nil
                           for num-tiles from min-tiles to max-tiles do
                             (when (or (iplusp num-tiles)
                                       (and (first gaps)
                                            (izerop (first gaps))
                                            (izerop num-tiles)))
                               (loop
                                  named :advance-to-next-empty-square do
                                    (multiple-value-setq (r c)
                                      (next-sq r c t dir))
                                    (when (or (not (on-board-p r c))
                                              (i= +empty+ (aref letters r c)))
                                      (return-from :advance-to-next-empty-square))))
                             (when debug-p
                               (format t "r: ~a c: ~a~%" r c))
                             (if (on-board-p r c)
                                 (let ((cross (cross-at r c dir)))
                                   (setf hooked-p (or hooked-p (iplusp cross))
                                         (aref scores num-tiles) score
                                         (aref positions num-tiles)
                                         (if (eq dir :horizontal)
                                             (i- c col)
                                             (i- r row))
                                         (aref patterns num-tiles)
                                         (if used-thrus
                                             (pattern used-thrus used-gaps)
                                             (if hooked-p
                                                 '(#.+segment-delimiter+
                                                   #.+segment-delimiter+)
                                                 nil)))
                                   (when debug-p
                                     (format t "  r: ~a c: ~a dir: ~a cross: ~a ~
                                              score: ~a hooked-p: ~a used-thrus: ~a ~
                                              used-gaps: ~a~%"
                                             r c dir cross score hooked-p
                                             used-thrus used-gaps)))
                                 (setf (aref patterns num-tiles) nil)))
                        (setf min-tiles (i1+ max-tiles))))
                 (values patterns scores positions))))

   (do-squares (row col dir sq-info h/v)
      (let ((letter (aref letters row col)))
        (when (i= +empty+ letter)
          (let* ((before (reverse (get-word row col nil dir)))
                 (after (get-word row col t dir))
                 (between (fit-between before after lexicon))
                 (before-score (get-score row col nil dir))
                 (after-score (get-score row col t dir)))
            (setf (h/v-cross h/v) between
                  (h/v-score h/v) (i+ before-score after-score))))
        #+nil
        (when (i/= +empty+ letter)
          (format t "row: ~a col: ~a dir: ~a~%" row col dir))))

   (do-squares (row col dir sq-info h/v)
     (multiple-value-bind (prev-r prev-c)
         (next-sq row col nil dir)
       (unless (and (on-board-p prev-r prev-c)
                    (iplusp (aref letters prev-r prev-c)))
         (multiple-value-bind (thrus gaps)
             (thru-words row col dir)
           (when debug-p
             (format t "row: ~a col: ~a dir :~a thrus: ~a gaps: ~a~%"
                     row col dir (mapcar #'list2text thrus) gaps))
           (multiple-value-bind (thru-patterns scores positions)
               (patterns thrus gaps row col dir)
             (when debug-p
               (format t "scores: ~a~%" scores))
             (setf (h/v-nodes h/v) (map 'vector
                                        (lambda (pattern)
                                          (or (filldawg-prefix-node-idx
                                               pattern 0 lexicon)
                                              0))
                                        thru-patterns)
                   (h/v-scores h/v) scores
                   (h/v-positions h/v) positions)
             #+nil
             (loop
                for pattern across thru-patterns
                for num-tiles fixnum from 1
                for node = (filldawg-prefix-node-idx pattern 0 lexicon)
                for example-lists = (and pattern debug-p
                                         (not (equal node 35168598))
                                         (filldawg-extensions
                                          nil node lexicon))
                for of-correct-length = (loop
                                           for list in example-lists
                                           when (i= (length list) num-tiles)
                                           collect list)
                for example-words =
                  (mapcar #'pattern2string
                          (subseq
                           of-correct-length 0
                           (min 10 (length of-correct-length))))
                do
                  (when debug-p
                    (format t "pattern: ~a num: ~a examples: ~a~%"
                            (pattern2string pattern)
                            num-tiles
                            example-words))))))))))))

(defstruct perm-elt
  change-pos
  pos-value
  next)

(defstruct perm-seq
  num-items
  elts)

(defvar *base-perm-seq-cache* (make-hash-table))
(defvar *unique-perm-seq-cache* (make-hash-table :test 'equal))

(defun new-perm-seq (n &optional (memoize-p t))
  (when memoize-p
    (return-from new-perm-seq
      (or (gethash n *base-perm-seq-cache*)
          (setf (gethash n *base-perm-seq-cache*)
                (new-perm-seq n nil)))))

  (let ((elts nil)
        (old-indices (make-array (list n)
				 :element-type 'fixnum
				 :initial-element -1))
	(indices (make-array (list n))))
    (dotimes (index n) (setf (aref indices index) index))
    (labels ((reverse-from (start)
	       (loop
		  for reverse1 from start below n
		  for reverse2 from (i1- n) downto start
		  while (i< reverse1 reverse2)
		  do (rotatef (aref indices reverse1)
			      (aref indices reverse2))))

	     ;; ported from C++ STL <algorithm>
	     (set-next-perm ()
	       (when (i> 2 n)
		 (return-from set-next-perm nil))
	       (loop
		  with i = (i1- n)
		  for j = i
		  do 
		    (idecf i)
		    (when (i< (aref indices i) (aref indices j))
		      (let ((k (i1- n)))
			(loop
			   while (i>= (aref indices i)
				      (aref indices k))
			   do (idecf k))
			(rotatef (aref indices i) (aref indices k))
			(reverse-from j)
			(return-from set-next-perm t)))
		    (when (izerop i)
		      (reverse-from 0)
		      (return-from set-next-perm nil)))))

      (loop :named changes do
	 (loop
	    with change-p = nil
	    for i below n do
	      (when (or change-p (i/= (aref indices i)
				      (aref old-indices i)))
		(let ((e (make-perm-elt
			  :change-pos i
			  :pos-value (aref indices i))))
		  (push e elts)
		  (setf change-p t))))
	 (dotimes (i n) (setf (aref old-indices i) (aref indices i)))
	 (unless (set-next-perm)
	   (return-from changes)))

      (dotimes (i n) (setf (aref indices i) (length elts)))

      (setf elts (make-array (list (length elts))
                             :initial-contents (nreverse elts)))

      (loop 
         with size = (length elts)
         for r from (i1- size) downto 0
         for elt = (aref elts r)
         do
           (setf (perm-elt-next elt)
                 (aref indices (perm-elt-change-pos elt)))
           (loop
              for q from (perm-elt-change-pos elt) below n do
                (setf (aref indices q) r)))
      
      (make-perm-seq :num-items n :elts elts))))

(defun get-perm-constraints (rack)
  (let* ((size (rack-size rack))
         (constraints (make-array size))
         (constraint-p nil)
         (same-indices 1))
    (dotimes (i size) (setf (aref constraints i) (cons 0 0)))
    (loop
       for i from 1 below size do
         (when (i/= (aref (rack-tiles rack) i)
                    (aref (rack-tiles rack) (i1- i)))
           (setf same-indices 0))
         (setf (car (aref constraints i)) same-indices)
         (when (iplusp same-indices)
           (setf constraint-p t))
         (set-bitf same-indices i))
    (values constraint-p constraints)))

(defun constraint-key (rack)
  (multiple-value-bind (constraint-p constraints)
      (get-perm-constraints rack)
    (declare (ignore constraint-p))
    (loop for (a . b) across constraints collecting a)))

(defun test-perm-constraints (constraints pos index)
  (let ((preceding-indices (if (izerop pos)
                               0
                               (cdr (aref constraints (i1- pos)))))
        (required-preceding-indices (car (aref constraints index))))
    (when (i/= (ilogand required-preceding-indices preceding-indices)
               required-preceding-indices)
      (return-from test-perm-constraints nil))
    (setf (cdr (aref constraints pos))
          (ilogior preceding-indices (1bit index)))))

(defun unique-rack-perms (rack &optional (memoize-p t))
  (when memoize-p
    (let ((key (constraint-key rack)))
      (return-from unique-rack-perms
        (or (gethash key *unique-perm-seq-cache*)
            (setf (gethash key *unique-perm-seq-cache*)
                  (unique-rack-perms rack nil))))))

  (let* ((base (new-perm-seq (rack-size rack) memoize-p)))
    (multiple-value-bind (constraint-p constraints)
        (get-perm-constraints rack)

      (when (null constraint-p)
        (return-from unique-rack-perms base))

      (let* ((seq (make-perm-seq :elts (copy-seq
                                        (perm-seq-elts base))))
             (orig-size (length (perm-seq-elts seq)))
             (orig2filtered (make-array (list (i1+ orig-size))))
             (j 0))

        (loop :named test-constraints
           with i = 0
           for elt = (aref (perm-seq-elts seq) i) do
             (if (test-perm-constraints constraints
                                        (perm-elt-change-pos elt)
                                        (perm-elt-pos-value elt))
                 (progn
                   (setf (aref orig2filtered i) j
                         (aref (perm-seq-elts seq) j) elt)
                   (iincf j)
                   (iincf i)
                   (when (i= i (length (perm-seq-elts seq)))
                     (return-from test-constraints)))
                 (progn
                   (setf (aref orig2filtered i) MOST-POSITIVE-FIXNUM)
                   (setf i (perm-elt-next elt))
                   (when (i= i (length (perm-seq-elts seq)))
                     (return-from test-constraints)))))

        (setf (perm-seq-elts seq)
              (adjust-array (perm-seq-elts seq) (list j)))

        (setf (aref orig2filtered orig-size) j)
        
        (loop
           for i from (i1- orig-size) downto 0
           for f = (aref orig2filtered i) do
             (when (i= MOST-POSITIVE-FIXNUM f)
               (let* ((elt (aref (perm-seq-elts base) i))
                      (next (perm-elt-next elt)))
                 (setf f (aref orig2filtered next))
                 (setf (aref orig2filtered i) f)
                 (assert (i>= next i))
                 (assert (i<= f j)))))

        (loop
           for i below j
           for elt = (aref (perm-seq-elts seq) i) do
             (assert (i<= (perm-elt-next elt) orig-size))
             (setf (perm-elt-next elt)
                   (aref orig2filtered (perm-elt-next elt)))
             (assert (<= (perm-elt-next elt) j))
             #+nil
             (format t "(perm-elt-next elt): ~a j: ~a perm-elt-change-pos-blah: ~a~%"
                     (perm-elt-next elt) j
                     (when (i/= (perm-elt-next elt) j)
                       (perm-elt-change-pos
                        (aref (perm-seq-elts seq)
                              (perm-elt-next elt)))))
             (assert (or (i= (perm-elt-next elt) j)
                         (>= (perm-elt-change-pos elt)
                             (perm-elt-change-pos
                              (aref (perm-seq-elts seq)
                                    (perm-elt-next elt)))))))
        
        seq))))
