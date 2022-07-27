(ns brave-clojure.euler)

;; Multiples of 3 or 5
;; Problem 1
;;
;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;;
;; Find the sum of all the multiples of 3 or 5 below 1000.
(#(or (= (mod % 3) 0)
      (= (mod % 5) 0)) 9)
;; => true

(reduce + (map (fn [n]
                 (if (or (= (mod n 3) 0)
                         (= (mod n 5) 0))
                   n
                   0))
               (range 1000)))
;; => 233168

;; Even Fibonacci numbers
;; Problem 2
;;
;; Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
;; 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
;;
;; By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
(#(= (mod % 2) 0) 1)
;; => false
(#(even? 4))
;; => true

(conj [1 2] 3)
;; => [1 2 3]

(reduce + (take-last 2 [1 2 3]))
;; => 5

(take 10 (reduce (fn [fibonacci n]
          (let [last-two-sum (reduce + (take-last 2 fibonacci))]
            (if (= n last-two-sum)
                (conj fibonacci last-two-sum)
                fibonacci)))
          [1 2]
          (range 4000000)))
;; => (1 2 3 5 8 13 21 34 55 89)

(reduce + (map (fn [n]
                 (if (even? n)
                   n
                   0))
               (range 4000000)))
;; => 3999998000000

(map (fn [n]
       (if (even? n)
         n
         0))
     [1 2 3 4])

(def fibonacci (reduce (fn [fibonacci n]
                         (let [last-two-sum (reduce + (take-last 2 fibonacci))]
                           (if (= n last-two-sum)
                             (conj fibonacci last-two-sum)
                             fibonacci)))
                       [1 2]
                       (range 4000000)))

(reduce + (map (fn [n]
                 (if (even? n)
                   n
                   0))
               fibonacci))
;; => 4613732

;; Largest prime factor
;; Problem 3
;;
;; The prime factors of 13195 are 5, 7, 13 and 29.
;;
;; What is the largest prime factor of the number 600851475143 ?
(/ 13195 29)
;; => 455

(mod 13195 29)
;; => 0

;; (defn factors
;;   [number]
;;   (loop [a 1
;;          b number
;;          factors []]
;;     (if (> a b)
;;       factors
;;       (recur (inc a)
;;              (/ number (inc a))
;;              (if (and (integer? (inc a))
;;                       (integer? (/ number (inc a))))
;;                (conj factors a b)
;;                factors)))))

;; 1x13195
;; 5x2639
;; axb
;; a>b
(defn factors
  [number]
  (reduce (fn [factors n]
            (if (< n (apply min (take-nth 2 (drop 1 factors))))
              (let [a n
                    b (/ number a)]
                (if (and (integer? a)
                         (integer? b))
                  (conj factors a b)
                  factors))
              (reduced factors)))
          [1 number]
          (range 1 number)))

(take-nth 2 (drop 1 [1 2 3 4]))
;; => (1 2 3)

(reduce + (range 10))
;; => 45

(reduce (fn [a v] (+ a v)) (range 10))
;; => 45

(reduce (fn [sub n]
          (if (< (reduce + sub) 100)
            (conj sub n)
            (reduced sub)))
        []
        (range 20))
;; => [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14]

(into #{} (factors 13195))
;; => #{65 377 7 13195 1 91 13 29 2639 35 145 5 1885 455 203 1015}

(into #{} (factors 600851475143))
;; => #{1
;;      6857
;;      486847
;;      1234169
;;      600851475143
;;      5753023
;;      716151937
;;      10086647
;;      104441
;;      87625999
;;      8462696833
;;      839
;;      408464633
;;      1471
;;      59569
;;      71}

(range 2 13)
;; => (2 3 4 5 6 7 8 9 10 11 12)

(every? false? [false false])
;; => true

(defn prime? [n]
  (every? false?
          (map #(= (mod n %) 0)
               (range 2 n))))

(prime? 14)
;; => false

(defn primes
  [numbers]
  (reduce (fn [prime n]
            (if (prime? n)
              (conj prime n)
              prime))
          []
          numbers))

(primes (into #{} (factors 600851475143)))
;; => [1 6857 839 1471 71]
