(in-package :ouat)

;; generalize this and make *english* default
(defun print-board (board &optional (str t))
  (let* ((squares (board-pretty-squares board *english*
                                        *standard-letter-mul-texts*
                                        *standard-word-mul-texts*))
         (layout (board-layout board)))
    (labels ((horizontal-border ()
               (format str "   ")
               (dotimes (i (i1- (i* 2 (layout-width layout))))
                 (format str "-"))
               (format str "~%")))
      (format str "   ")
      (loop
         for col below (layout-width layout)
         for label = (code-char (i+ (char-code #\A) col))
         do (format str "~a " label))
      (format str "~%")
      (horizontal-border)
      (loop
         for row below (layout-height layout)
         for row-squares in squares
         for pretty-row = (i1+ row)
         do
           (if (i> 10 pretty-row)
               (format str " ~a|" pretty-row)
               (format str "~a|" pretty-row))
           (loop
              for (square . rest) on row-squares
              do
                (format str "~a" square)
                (if rest 
                    (format str " ") 
                    (format str "|~%"))))
      (horizontal-border))))

;; only supports single character per tile
;; assumes that string-upcase is a reasonable thing to do
(defun new-rack (string &key (capacity 7) (tiles *english*))
  (let* ((tile-array (make-array (list capacity)
                                 :element-type 'fixnum
                                 :initial-element +empty+))
         (size 0))
    (loop
       for text-char across (string-upcase string)
       for text = (format nil "~a" text-char)
       for tile = (text-tile text tiles)
       do (if tile
              (progn
                (setf (aref tile-array size) tile)
                (iincf size))
              (format t "Unknown tile: ~a~%" text)))
    (make-rack :capacity capacity
               :tiles (adjust-array tile-array `(,size)))))