(defmacro color [name]
  (let [long-name# (str "SWT/COLOR_" (.toUpperCase name))]
    `(.getSystemColor *display* ~(symbol long-name#))))

(defmacro nary-or [first & rest]
  (if rest
    `(bit-or ~first (nary-or ~@rest))
    first))

(defmacro unless [condition & body]
  `(when (not ~condition) ~@body))

(defmacro async [body]
  `(.asyncExec *display* (proxy [Runnable] []
                           (run [] ~@body))))

(defmacro random [radix]
  `(.nextInt (java.util.Random.) ~radix))

(defn downcase [string]
  (.toLowerCase string))

(defn upcase [string]
  (.toUpperCase string))

