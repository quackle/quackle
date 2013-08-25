(def *question-html-header* "<body bgcolor=\"#eeeeee\"><center><table border=1 cellpadding=10>")
;(def *question-html-header* "<body bgcolor=\"#eeeeee\"><center><table border=0 cellpadding=1>")

(defn word-solved? [word]
  (nil? (re-find #"[A-Z]" word)))

(defn word-unsolved? [word]
  (not (word-solved? word)))

(defn ana-set-solved? [ana-set]
  (every? word-solved? ana-set))

(defn alphagram [ana-set order-string]
  (apply str (sort-by #(.indexOf order-string (str %))
                      (upcase (first ana-set)))))

(defn word-space [word]
  (apply str (take (.length word) (repeat "&#x2012;"))))

(defn gridquiz-ana-set-html [ana-set]
  (loop [html (str "<center><b>
                    <p style=\"font-family: bitstream vera sans;
                               font-size: 240%; 
                               margin-top: 0;
                               color: " (if (ana-set-solved? ana-set) 
                                          "#a0a0a0;" 
                                          "#000000;")
                              "margin-bottom: 0.25em\">" 
                   (alphagram ana-set *vowel-first-order*) 
                   "</p></b>")
         [word & more-words] ana-set]
    (let [word-text (if (word-solved? word) 
                      (upcase word)
                      (word-space word))
          word-html (str "<p style=\"font-family: bitstream vera sans;
                                     font-size: 172%;
                                     margin-top: 0;
                                     margin-bottom: 0\">"
                         word-text
                         "<br>")
          new-html (str html word-html)]
      (if (empty? more-words)
        new-html
        (recur new-html more-words)))))

(defn pagequiz-ana-set-html [ana-set]
  (loop [html (if (ana-set-solved? ana-set)
                ""
                (str "<center>
                      <p style=\"font-family: bitstream vera sans mono;
                                 font-size: 80%; 
                                 color: #000000;
                                 margin-top: 0;
                                 margin-left: 15px;
                                 margin-right: 15px;
                                 margin-bottom: 0.25em\">" 
                      (alphagram ana-set *vowel-first-order*) 
                      "</p>"))
         [word & more-words] ana-set]
    (let [word-text (upcase word)
          word-html (str "<p style=\"font-family: bitstream vera sans mono;
                                     font-size: 80%;
                                     color: #a0a0a0;
                                     margin-top: 0;
                                     margin-bottom: 0;
                                     margin-left: 15px;
                                     margin-right: 15px\">"
                         word-text
                         "<br>")
          new-html (if (ana-set-solved? ana-set) 
                     (str html word-html)
                     html)]
      (if (empty? more-words)
        new-html
        (recur new-html more-words)))))
    
(defn board-html [ana-set-html-fn]
  (loop [index 0
         html *question-html-header*
         [ana-set & more-ana-sets] @*board*]
    (let [h-pos (rem index *default-gridquiz-width*)
          set-html (str (if (zero? h-pos) "<tr>" "") "<td valign=\"top\">"
                        (ana-set-html-fn ana-set) "</td>"
                        (if (= h-pos (dec *default-gridquiz-width*)) "</tr>" ""))
          new-html (str html set-html)]
      (if (empty? more-ana-sets)
        new-html
        (recur (inc index) new-html more-ana-sets)))))

(defn ana-sets-of-length [length lexicon]
  (let [filename (str *wordmonger-path* "words/" lexicon "/length" length ".txt")
        sequence (read-lines filename)]
    (map #(re-seq #"\S+" %) sequence)))
    
(defn random-board [length board-size lexicon]
  (let [ana-sets (ana-sets-of-length length lexicon)
        num-sets (count ana-sets)
        sets-array (to-array ana-sets)]
     (loop [board [], num-more board-size]
      (let [random-set (aget sets-array (random num-sets))]
        (if (contains? (set board) random-set)
          (recur board num-more)
          (if (pos? num-more)
            (recur (conj board random-set) (dec num-more))
            board))))))

(defn set-board-to-random-board [length lexicon]
  (let [board (random-board length 
                            (* *default-gridquiz-height*
                               *default-gridquiz-width*)
                            lexicon)]
    (def *board* (ref board))))

(defn write-results []
  (let [millis (System/currentTimeMillis)
        yyyy-mm-dd (.toString (new java.sql.Date millis))
        time (new java.sql.Time millis)
        h (.getHours time), m (.getMinutes time), s (.getSeconds time)
        hh (if (> 10 h) (str "0" h) h)
        mm (if (> 10 m) (str "0" m) m)
        ss (if (> 10 s) (str "0" s) s)
        filename (str "results/" yyyy-mm-dd "-" hh mm ss ".txt")
        words (apply concat @*board*)
        misses (filter word-unsolved? words)
        found (filter word-solved? words)
        result (str @*timer-value* " sec left out of " *gridquiz-timer-max*
                    ", missed " (count misses) " of " (count words))]
    (write-lines filename (concat [result] words))))

(defn replace-in-anagram-set [answer anagram-set]
  (loop [checked-words []
         [word & more-words] anagram-set]
    (if (.equals word answer)
      (let [new-set (concat checked-words (cons (downcase word) more-words))]
        (list true new-set))
      (let [new-checked-words (concat checked-words (list word))]
        (if (empty? more-words)
          (list false new-checked-words)
          (recur new-checked-words more-words))))))

(defn replace-in-board [answer]
  (loop [checked-sets []
         [anagram-set & more-sets] @*board*]
    (let [[changed new-set] (replace-in-anagram-set answer anagram-set)]
      (if changed
        (let [new-board (concat checked-sets (cons new-set more-sets))]
          (list true new-board))
        (let [new-checked-sets (concat checked-sets (list anagram-set))]
          (if (empty? more-sets)
            (list false new-checked-sets)
            (recur new-checked-sets more-sets)))))))

(defn replace-in-board! [answer]
  (let [[changed new-board] (replace-in-board answer)]
    (when changed
      (def *board* (ref new-board)))))

(defn reveal-answers []
  (doseq [answer (map upcase (apply concat @*board*))]
    (replace-in-board! answer))
  
  (.asyncExec *display* 
              (proxy [Runnable] []
                (run [] (.setText *question-browser* 
                                  (board-html #'gridquiz-ana-set-html)))))
  (comment
    (async (.setText *question-browser* (board-html #'gridquiz-ana-set-html)))))

(defn game-over []
  (write-results)
  (reveal-answers)
  (comment
    (async (.setText *answer-text* ""))))

(defn start-timer []
  (initialize-timer *gridquiz-timer-max*)
  (.start (Thread. (fn []
                       (loop []
                          (Thread/sleep 1000)
                          (when (not-every? word-solved? 
                                            (apply concat @*board*))
                            (decrement-timer))
                          (if (zero? @*timer-value*)
                              (game-over)
                              (recur)))))))

(defn twl06-sevens-gridquiz []
  (set-board-to-random-board 7 "twl06")
  (.setText *answer-text* "")
  (.setText *question-browser* (board-html #'gridquiz-ana-set-html))
  (start-timer))

(defn csw07-single-eights-gridquiz []
  (set-board-to-random-board 8 "csw07")
  (.setText *question-browser* (board-html #'gridquiz-ana-set-html))
  (start-timer))

(defn csw07-single-eights-pagequiz []
  (set-board-to-random-board 8 "csw07")
  (.setText *question-browser* (board-html #'pagequiz-ana-set-html))
  (start-timer))

(doto *question-browser*
  (.setFont (new Font *display* "bitstream vera sans" 12 SWT/NORMAL))
  (.setLayoutData (new GridData (nary-or GridData/HORIZONTAL_ALIGN_FILL
                                         GridData/GRAB_HORIZONTAL
                                         GridData/VERTICAL_ALIGN_FILL
                                         GridData/GRAB_VERTICAL)))
  (.addListener SWT/KeyDown (handle-key-listener)))

(defn answer-modify [event]
  (let [[changed new-board] (replace-in-board (.getText *answer-text*))]
    (when changed
      (def *board* (ref new-board))
      (.setText *answer-text* "")
      (.setText *question-browser* 
                (board-html #'gridquiz-ana-set-html))
      (when (every? word-solved? (apply concat @*board*))
        (write-results)))))

(defn answer-modify-listener []
  (proxy [ModifyListener] []
    (modifyText [event] (answer-modify event))))

(defn answer-verify [event]
  (let [key (.keyCode event)]
    (cond (<= (int \a) key (int \z))
            (set! (.text event) (upcase (.text event)))
          (and (not= key (int \backspace)) (not (zero? key)))
            (set! (.doit event) false))))
 
(defn answer-verify-listener []
  (proxy [VerifyListener] []
    (verifyText [event] (answer-verify event))))

(doto *answer-text*
  (.setFocus)
  ;(.addListener SWT/KeyDown (key-listener))
  (.addFocusListener (focus-listener))
  (.addModifyListener (answer-modify-listener))
  ;(.addVerifyListener (answer-verify-listener))
)
