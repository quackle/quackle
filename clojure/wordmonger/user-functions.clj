(defn menu-bar-mode []
  (println "menu-bar-mode"))

(defn split-horizontally []
  (println "split-horizontally"))

(defn split-vertically []
  (println "split-vertically"))

(defn remove-split []
  (println "remove-split"))

(defn keyboard-quit []
  (println "keyboard-quit"))

(defn gridquiz []
  (println "gridquiz"))

(defn letterbox []
  (println "letterbox"))

(defn describe-key []
  (println "describe-key"))

(defn help []
  (println "help"))

(defn customize []
  (println "customize"))

(defn move-beginning-of-line []
  ;(println "focus: " @*focused-widget*)
  (.setCaretOffset @*focused-widget* 0))

(defn move-end-of-line []
  (.setCaretOffset @*focused-widget* (.getCharCount @*focused-widget*)))

(def *key-bindings*
     {;"M-x"  #'mode-line-prompt
      "C-xh" #'split-vertically
      "C-xv" #'split-horizontally
      "C-xr" #'remove-split
      "C-a"  #'move-beginning-of-line
      "C-e"  #'move-end-of-line})

