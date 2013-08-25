(use 'clojure.contrib.duck-streams)

(import '(org.eclipse.swt SWT))
(import '(org.eclipse.swt.browser Browser))
(import '(org.eclipse.swt.custom StyleRange StyledText VerifyKeyListener))
(import '(org.eclipse.swt.events ModifyListener VerifyListener FocusEvent
                                 FocusListener))
(import '(org.eclipse.swt.graphics Color Font RGB))
(import '(org.eclipse.swt.layout GridData GridLayout))
(import '(org.eclipse.swt.widgets Display Label Listener Menu MenuItem Shell Text
                                  Widget))

(def *display* (new Display))
(def *shell* (new Shell *display*))

(def *question-browser* (new Browser *shell* SWT/MOZILLA))
(def *timer-label* (new Label *shell* SWT/CENTER))
;(def *answer-text* (new Text *shell* SWT/BORDER))
(def *answer-text* (new StyledText *shell* SWT/BORDER))
;(def *minibuffer* (new StyledText *shell* SWT/NONE))

;(def *wordmonger-path* "/home/john/sources/quackle/qcvs-nov11-2008/clojure/wordmonger/")
(def *wordmonger-path* "./")
(def *question-html-header* "<body bgcolor=\"#eeeeee\"><center><table border=1 cellpadding=10>")
(def *alphabetical-order* "ABCDEFGHIJKLMNOPQRSTUVWXYZ?")
(def *vowel-first-order* "AEIOUBCDFGHJKLMNPQRSTVWXYZ?")

(def *default-gridquiz-height* 5)
(def *default-gridquiz-width* 6)
(def *board* [])
(def *gridquiz-timer-max* 80)
(def *timer-value* nil)
(def *focused-widget* nil)

;(load-file "menu.clj")
(load-file "macros.clj")
(load-file "styled-text.clj")
(load-file "user-functions.clj")
(load-file "listeners.clj")
(load-file "timer.clj")
 
(comment 
  (doto *minibuffer*
    (.setFont (new Font *display* "bitstream vera sans mono" 8 SWT/NORMAL))
    (.setLayoutData (new GridData GridData/HORIZONTAL_ALIGN_FILL))
    (.addFocusListener (focus-listener))
    (.addListener SWT/KeyDown (key-listener))))

(doto *answer-text*
  (.setFont (new Font *display* "bitstream vera sans" 12 SWT/NORMAL))
  (.setText "                       ")
  (.setLayoutData (new GridData GridData/HORIZONTAL_ALIGN_CENTER))
  ;(.addListener SWT/KeyDown (key-listener))
  (.addVerifyKeyListener (verify-key-listener))
  )

(load-file "gridquiz.clj")

(let [grid-layout (new GridLayout)]
  (doto *shell*
    (.setLayout grid-layout)
    ;(.setMenuBar *menu-bar*)
    (.setBackground (new Color *display* 238 238 238))
    (.setText "Wordmonger")
    ;(.addListener SWT/KeyDown (key-listener))
    (.pack)
    (.open)))

(twl06-sevens-gridquiz)
;(csw07-single-eights-gridquiz)
;(csw07-single-eights-pagequiz)

(loop []
  (if (.isDisposed *shell*)
    (.dispose *display*)
    (do (when (not (.readAndDispatch *display*))
          (.sleep *display*))
        (recur))))