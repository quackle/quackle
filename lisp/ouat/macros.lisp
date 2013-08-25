(in-package :ouat)

(macrolet 
    ((def-unary-op (name generic)
       `(defmacro ,name (arg)
	  `(,',generic (the fixnum ,arg))))
           
     (def-unary-32-op (name generic)
       `(defmacro! ,name (arg)
	  `(,',generic (the (unsigned-byte 32) ,arg))))
     
     (def-unary-fixnum-op (name generic)
       `(defmacro ,name (arg)
	  `(the fixnum (,',generic (the fixnum ,arg)))))
           
     (def-binary-op (name generic)
       `(defmacro ,name (left right)
	  `(,',generic (the fixnum ,left) (the fixnum ,right))))
           
     (def-binary-fixnum-op (name generic)
       `(defmacro ,name (left right)
	  `(the fixnum (,',generic (the fixnum ,left) (the fixnum ,right)))))
           
     (def-nary-op (name generic)
       `(defmacro ,name (&rest args)
	  `(,',generic ,@(mapcar (lambda (arg) `(the fixnum ,arg)) args))))
     
     (def-nary-fixnum-op (name generic)
       `(defmacro ,name (&rest args)
	  `(the fixnum (,',generic ,@(mapcar (lambda (arg) `(the fixnum ,arg)) args))))))

  (def-unary-op ievenp evenp)
  (def-unary-op ilogcount logcount)
  (def-unary-op ioddp oddp)
  (def-unary-op izerop zerop)
  (def-unary-op iplusp plusp)
  (def-unary-op iminusp minusp)

  (def-unary-fixnum-op i1+ 1+)
  (def-unary-fixnum-op i1- 1-)
  (def-unary-fixnum-op inegate -)
  (def-unary-fixnum-op iabs abs)
  (def-unary-fixnum-op ilognot lognot)

  (def-nary-op i= =)
  (def-nary-op i/= /=)
  (def-nary-op i< <)
  (def-nary-op i> >)
  (def-nary-op i<= <=)
  (def-nary-op i>= >=)
  (def-binary-op ilogtest logtest)

  (def-binary-fixnum-op i- -)
  (def-binary-fixnum-op i/ floor)
  (def-nary-fixnum-op i+ +)
  (def-nary-fixnum-op i* *)
  (def-nary-fixnum-op iincf incf)
  (def-nary-fixnum-op idecf decf)

  (def-binary-fixnum-op iash ash)
  (def-binary-fixnum-op ilogandc1 logandc1)
  (def-binary-fixnum-op ilogandc2 logandc2)
  (def-binary-fixnum-op ilognand lognand)
  (def-binary-fixnum-op ilognor lognor)
  (def-binary-fixnum-op ilogorc1 logorc1)
  (def-binary-fixnum-op ilogorc2 logorc2)

  (def-nary-fixnum-op ilogand logand)
  (def-nary-fixnum-op ilogior logior)
  (def-nary-fixnum-op ilogxor logxor)
  (def-nary-fixnum-op imax max)
  (def-nary-fixnum-op imin min)

  (def-binary-fixnum-op irem rem)
  (def-binary-fixnum-op iround round)
  (def-binary-fixnum-op itruncate truncate))  