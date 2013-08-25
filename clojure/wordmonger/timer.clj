(doto *timer-label*
  (.setFont (new Font *display* "bitstream vera sans mono" 12 SWT/NORMAL))
  (.setText "   ")
  (.setLayoutData (new GridData GridData/HORIZONTAL_ALIGN_CENTER)))

(defn initialize-timer [timer-max]
  (def *timer-value* (ref timer-max)))

(defn decrement-timer []
  (let [value @*timer-value*
        new-value (dec value)]
    (unless (.isDisposed *shell*)
        (.asyncExec *display* (proxy [Runnable] []
                                (run [] (.setText *timer-label* 
                                                  (str new-value)))))
      (comment
        (async (.setText *timer-label* (str new-value))))
      (def *timer-value* (ref new-value)))))
