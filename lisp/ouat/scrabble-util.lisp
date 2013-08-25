(in-package :ouat)

(defun row2uv-row (row)
  (format nil "~A" (i1+ row)))

(defun col2uv-col (col)
  (string (code-char (i+ #.(char-code #\A) col))))

(defun uv-row2row (row)
  (i1- (parse-integer row)))

(defun uv-col2col (col)
  (i- (char-code (elt col 0))
      #.(char-code #\A)))

(defconstant +empty+ 0)
(defconstant +all-letters+ -1)