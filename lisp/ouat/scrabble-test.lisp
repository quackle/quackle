(in-package :ouat)

(defun test-opening-tops ()
  (do-file-lines (line (string-append *test-directory* "racks.txt"))
    (let* ((rack (new-rack line))
           (tops (topsquiet nil rack nil))
           (score (if tops (move-score (first tops)) 0))
           (tops-strings (mapcar (lambda (move)
                                   (move-string move *english*))
                                 tops))
           (answer (list :rack line
                         :score score
                         :tops tops-strings))
           (*print-pretty* nil))
    (format t "~s~%" answer))))

(defun test-sq-infos ()
  (let* ((b (new-board *standard*))
         (re (make-move :action :place
                        :direction :horizontal
                        :start-row 7
                        :start-col 6
                        :word #(18 5)
                        :blank-ps #(nil nil)
                        :already-on-board-ps #(nil nil)))
         (as (make-move :action :place
                        :direction :vertical
                        :start-row 2
                        :start-col 5
                        :word #(1 19)
                        :blank-ps #(nil nil)
                        :already-on-board-ps #(nil nil)))
         (in (make-move :action :place
                        :direction :horizontal
                        :start-row 7
                        :start-col 10
                        :word #(9 14)
                        :blank-ps #(nil nil)
                        :already-on-board-ps #(nil nil)))
         (d (make-move :action :place
                       :direction :horizontal
                       :start-row 7
                       :start-col 13
                       :word #(4)
                       :blank-ps #(nil)
                       :already-on-board-ps #(nil))))
    (place-move b re)
    (place-move b in)
    (place-move b d)
    (place-move b as)
    #+nil (print-board b)
    (update-sq-infos b *twl* *english* nil)
    (format t "sq-infos: ~a~%" (board-sq-infos b))))

         
