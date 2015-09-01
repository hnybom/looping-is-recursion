(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n] (if (zero? n)
                             acc
                             (recur (* acc base) (dec n))))]
    (if (zero? exp)
      1
      (helper base (dec exp)))))

(defn last-element [a-seq]
  (let [helper (fn [seq-b] (if (= (count seq-b) 1)
                 (first seq-b)
                 (recur (rest seq-b))))]
    (if (empty? a-seq)
      nil
      (helper a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [h-s1 h-s2]
                 (cond
                   (and (empty? h-s1) (empty? h-s2)) true
                   (= (first h-s1) (first h-s2)) (recur (rest h-s1) (rest h-s2))
                   :else false))]
    (if (not= (count seq1) (count seq2))
      false
      (helper seq1 seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         length (count a-seq)]
    (cond
      (>= n length) nil
      (pred (get a-seq n)) n
      :else (recur (inc n) length))))

(defn avg [a-seq]
  (loop [seq a-seq
         acc 0]
    (cond
      (empty? seq) (/ acc (count a-seq))
      :else (recur (rest seq) (+ acc (first seq))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq a-seq
         result-set #{}]
    (if (empty? seq)
      result-set
      (recur (rest seq) (toggle result-set (first seq))))))

(defn fast-fibo [n]
  (loop [ln 0
         ln-1 1
         ind 0]
    (if (= ind n)
      ln
      (recur (+ ln ln-1) ln (inc ind)))))

(defn cut-at-repetition [a-seq]
  (loop [new-seq []
         go-seq a-seq]
    (let [f (first go-seq)]
         (if (or (some (fn [x] (= x f)) new-seq) (empty? go-seq))
           new-seq
           (recur (conj new-seq f) (rest go-seq))))))

