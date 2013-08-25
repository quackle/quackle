(in-package :ouat)

(defun tile-image (file letter subscr
                   &key (size 100) (hoffset 0))
  (let* ((radius (* 0.05 size))
         (normal-letter-height (* 0.76 size))
         (letter-height normal-letter-height)
         (letter-voffset (* 0.5 (- normal-letter-height
                                   letter-height)))
         (normal-subscr-height (* 0.2 size))
         (subscr-height normal-subscr-height)
         (subscr-voffset (* 0.5 (- normal-subscr-height
                                   subscr-height))))
  (with-canvas (:width size :height size)
    (set-gradient-fill 0 0 
                       0.33 0.33 0.33 1
                       size size
                       0.53 0.53 0.53 1)
    (rounded-rectangle 0 0 size size radius radius)
    (fill-path)
    (set-rgb-fill 0.97 0.97 1.0)
    (set-font (get-font "/home/john/fonts/VeraBd.ttf") letter-height)
    (draw-centered-string (* (+ 0.44 hoffset) size)
                          (+ (* 0.28 size) letter-voffset)
                          letter)
    (set-font (get-font "/home/john/fonts/VeraBd.ttf") subscr-height)
    (set-rgba-fill 0.97 0.97 1.0 0.6)
    (draw-centered-string (* 0.83 size)
                          (+ (* 0.1 size) subscr-voffset) 
                          subscr)
    (save-png file))))

(defun tile-distribution-images (tile-distribution
                                 &key prefix (size 100))
  (loop
     for tile across tile-distribution
     when tile do
       (let ((file (format nil "~a~a.png"
                           (or prefix "") (tile-name tile)))
             (letter (or (tile-text tile) "?"))
             (subscr (format nil "~a" (tile-score tile))))
         (tile-image file letter subscr :size size))))