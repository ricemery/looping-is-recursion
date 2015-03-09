(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (- n 1))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc xs]
                 (if (empty? xs)
                   acc
                   (recur (first xs) (rest xs))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
     (and (empty? seq1) (empty? seq2)) true
     (empty? seq1) false
     (empty? seq2) false
     (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
     :else false))


(defn find-first-index [pred a-seq]
  (loop [acc 0
         xs a-seq]
    (cond
       (empty? xs) nil
       (pred (first xs)) acc
       :else (recur (+ acc 1) (rest xs)))))

(defn avg [a-seq]
  (loop [sumSeq 0
         numSeq 0
         xs a-seq]
    (cond
       (empty? xs) (if (= numSeq 0)
                     0
                     (/ sumSeq numSeq))
       :else (recur (+ sumSeq (first xs)) (+ numSeq 1) (rest xs)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         xs a-seq]
    (if (empty? xs)
      acc
      (recur (toggle acc (first xs)) (rest xs)))))

(defn fast-fibo [n]
  (loop [acc n
         a 0
         b 1]
    (if (= acc 0)
      a
      (recur (- acc 1) b (+ a b)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         seen #{}
         xs a-seq]
    (if (empty? xs)
      acc
      (if (contains? seen (first xs))
        acc
        (recur (conj acc (first xs)) (conj seen (first xs)) (rest xs))))))

