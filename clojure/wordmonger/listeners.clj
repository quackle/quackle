(defn handle-key [event]
  (println event)
  (let [mask (.stateMask event)
        state-string (str (if (zero? (bit-and mask SWT/ALT)) 
                            "" "M-")
                          (if (zero? (bit-and mask SWT/CTRL)) 
                            "" "C-"))
        shift-p (not (zero? (bit-and mask SWT/SHIFT)))
        key (.keyCode event)]
    (when (and (.isEmpty state-string)
               (= @*focused-widget* *answer-text*))
      (let [upcase-char (Character/toUpperCase (char key))
            upcase-code (int upcase-char)]
        (println "you typed" upcase-char "into *answer-text*")
        (when (<= (int \A) upcase-code  (int \Z))
          (set! (.doit event) false)
          (append-styled-text *answer-text* (str upcase-char) 
                              {:fg (color "black")}))))
    (unless (or (zero? key) (= key (int \backspace)))
      (let [key-string (str state-string (char key))
            key-fn (get *key-bindings* key-string)]
        (println "key:" key-string)
        (println "key-fn:" key-fn)
        (when key-fn (key-fn))))))

(defn handle-key-listener []
  (proxy [Listener] []
    (handleEvent [event] (handle-key event))))

(defn verify-key-listener []
  (proxy [VerifyKeyListener] []
    (verifyKey [event] (handle-key event))))

(defn handle-focus-gained [event]
  (def *focused-widget* (ref (.widget event))))

(defn focus-listener []
  (proxy [FocusListener] []
    (focusGained [event] (handle-focus-gained event))
    (focusLost [event])))

