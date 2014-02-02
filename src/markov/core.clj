(ns markov.core)

(def order 5)

(defn rand-occurence
  [freq-map]
  (let [total-count (reduce + (vals freq-map))
        n (rand-int total-count)]
      (loop [sum 0
             [[item freq] & tail] (seq freq-map)]
        (if (> (+ sum freq) n)
          item
          (recur (+ sum freq) tail)))))

(defn get-next [network text]
  (if (< (count text) order)
    (key (rand-nth (seq network)))
    (let [key (apply str (take-last order text))
          item (get network key ::not-found)]
      (when (not= item ::not-found)
        (rand-occurence item)))))

(defn make-network-deluxe [text]
    (loop [network {}
           left text]
      (let [key (apply str (take order left))
            occurence (nth left order)]
        (if (<= (count left) (inc order))
          network
          (recur (update-in network [key occurence] (fnil inc 0)) (rest left))))))

(defn slurp-n-fix [path]
  (apply str (apply concat (re-seq #"[A-z]+ " (slurp path)))))

(def tom-sawyer (slurp-n-fix "tom.txt"))
;(def miserables (slurp-n-fix "http://www.gutenberg.org/cache/epub/135/pg135.txt"))

tom-sawyer
;miserables

(count tom-sawyer)
;(count miserables)

(time (def network-1 (make-network-deluxe tom-sawyer)))
;(time (def network-2 (make-network-deluxe miserables)))

(defn get-nexter [network text]
  (let [c (get-next network text)]
    (str text c)))

(defn markov [network]
  (first (drop 100 (iterate #(get-nexter network %) ""))))

(println (markov network-1))





;; OLD AND SLOW NETWORK GENERATOR:
;; (defn step [network text]
;;   (let [key (apply str (take order text))
;;         occurence (nth text order)]
;;     (update-in network [key occurence] (fnil inc 0))))

;; (defn make-network [text]
;;   (let [end-point (- (count text) order)
;;         texts (mapv #(drop % text) (range end-point))]
;;     (reduce step {} texts)))
