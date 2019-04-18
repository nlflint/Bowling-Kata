(defn score [rolls]
  (cond
    (eq (car rolls) "10") "strike"
    (eq (+ (car rolls) (cadr rolls) 10) "spare")))

(score `(10))