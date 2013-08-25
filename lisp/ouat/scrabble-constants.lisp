(in-package :ouat)

(defvar *english-scrabble-bag*
  #.(format nil
            "AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHJKLLLLMMNNNNNN~
             OOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ??"))

(defvar *standard-board-eighth*
  (list "3W"
        ".. 2W"
        ".. .. 2W"
        "2L .. .. 2W"
        ".. .. .. .. 2W"
        ".. 3L .. .. .. 3L"
        ".. .. 2L .. .. .. 2L"
        "3W .. .. 2L .. .. .. 2W"))

(defvar *standard*
  (new-symmetric-square-layout *standard-board-eighth*))

(defvar *english-tile-list*
  '(("A"  9  1 :VOWEL)
    ("B"  2  3 :CONSONANT)
    ("C"  2  3 :CONSONANT)
    ("D"  4  2 :CONSONANT)
    ("E" 12  1 :VOWEL)
    ("F"  2  4 :CONSONANT)
    ("G"  3  2 :CONSONANT)
    ("H"  2  4 :CONSONANT)
    ("I"  9  1 :VOWEL)
    ("J"  1  8 :CONSONANT)
    ("K"  1  5 :CONSONANT)
    ("L"  4  1 :CONSONANT)
    ("M"  2  3 :CONSONANT)
    ("N"  6  1 :CONSONANT)
    ("O"  8  1 :VOWEL)
    ("P"  2  3 :CONSONANT)
    ("Q"  1 10 :CONSONANT)
    ("R"  6  1 :CONSONANT)
    ("S"  4  1 :CONSONANT)
    ("T"  6  1 :CONSONANT)
    ("U"  4  1 :VOWEL)
    ("V"  2  4 :CONSONANT)
    ("W"  2  4 :CONSONANT)
    ("X"  1  8 :CONSONANT)
    ("Y"  2  4 :SOMETIMES)
    ("Z"  1 10 :CONSONANT)
    ("?"  2  0 :BLANK :name "blank")))

(defun create-tile-distribution (tile-list)
  (let* ((num-tiles (length tile-list))
         (tile-array (make-array (list (i1+ num-tiles))
                                 :initial-element nil)))
    (loop
       for tile in tile-list
       for index from 1 do
         (destructuring-bind (text quantity score type
                                   &key blank-text name) 
             tile
           (setf blank-text (or blank-text
                                (and text (string-downcase text)))
                 name (or name text)
                 (aref tile-array index)
                 (make-tile :text text
                            :blank-text blank-text
                            :quantity quantity
                            :score score
                            :type type
                            :name name))))
    tile-array))
                                  
(defvar *english*
  (create-tile-distribution *english-tile-list*))

(defvar *standard-letter-mul-texts* #(nil nil "'" "\"" "^" "`"))
(defvar *standard-word-mul-texts*   #(nil "." "-" "="  "~" "+"))

(defvar *twl-dawg* (new-dawg *twl-dawg-nodes*))
(defvar *twl-filldawg-nodes* nil)

(open-dawg *twl-filldawg-nodes* 
           (string-append *data-directory* "twl-2to8.filldawg"))
           
(defvar *twl-filldawg* (make-filldawg
                        :nodes *twl-filldawg-nodes*))

(defvar *twl* (make-lexicon :dawg *twl-dawg*
                            :filldawg *twl-filldawg*))

(defvar *vanilla* (make-rules :bingo-bonus 50
                              :max-scoreless-turns 6
                              :challenge-rule :double
                              :overdraw-rule :naspa
                              :time-limit '(25 . 0)
                              :time-penalty-duration '(1 . 0)
                              :time-penalty 10))