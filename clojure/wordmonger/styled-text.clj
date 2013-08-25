(defn append-styled-text [styled-text text {:keys [fg style]
                                            :or {style SWT/NORMAL}}]
  (.append styled-text text)
  (let [new-offset (.getCharCount styled-text)
        length (min new-offset (.length text))
        start (max 0 (- new-offset length))
        style-range (new StyleRange start length fg nil style)]
    (println length new-offset start)
    (when (pos? length)
      (doto styled-text
        (.setStyleRange style-range)
        (.setCaretOffset new-offset)))))

(comment
  (defn mode-line-prompt []
    (doto *minibuffer*
      (.setText "")
      (append-styled-text "M-x " {:fg (color "blue")})
      (.setFocus))))

