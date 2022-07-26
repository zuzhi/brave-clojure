(ns brave-clojure.euler)


;; Multiples of 3 or 5
;; Problem 1
;;
;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;;
;; Find the sum of all the multiples of 3 or 5 below 1000.
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

(def fibonacci (reduce (fn [fibonacci n]
                         (let [new-term (reduce + (take-last 2 fibonacci))]
                           (if (= n new-term)
                             (conj fibonacci new-term)
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

(defn prime? [n]
  (every? false?
          (map #(= (mod n %) 0)
               (range 2 n))))

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


;; Largest palindrome product
;; Problem 4
;;
;; A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;;
;; Find the largest palindrome made from the product of two 3-digit numbers.

(defn my-reverse [n]
  (Integer/parseInt (apply str (reverse (str n)))))

(defn palindrome? [n]
  (= n (my-reverse n)))

(palindrome? 1234)
;; => false

(palindrome? 9009)
;; => true

(palindrome? (* 91 99))
;; => true

(def products (reduce (fn [coll a]
                        (loop [b a
                               coll coll]
                          (if (< b 100)
                            coll
                            (recur (dec b) (conj coll (* a b))))))
                      []
                      (range 999 99 -1)))

(apply max (reduce (fn [coll n]
                     (if (palindrome? n)
                       (conj coll n)
                       coll))
                   []
                   products))
;; => 906609


;; Smallest multiple
;; Problem 5
;;
;; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;;
;; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(defn numbers
  ([] (numbers 20))
  ([n] (cons n (lazy-seq (numbers (+ n 20))))))

(take 10 (numbers))

(range 2 21)

(defn sn?
  [n]
  (every? true?
          (reduce (fn [coll a]
                    (conj coll (integer? (/ n a))))
                  []
                  (range 2 21))))

(sn? 2520)

(reduce (fn [a b]
          (if (sn? b)
            (reduced b)
            nil))
        (numbers))
;; => 232792560


;; Sum square difference
;; Problem 6
;;
;; The sum of the squares of the first ten natural numbers is,
;; 1^2 + 2^2 + ... + 10^2 = 385
;; The square of the sum of the first ten natural numbers is,
;; (1 + 2 + ... + 10)^2 = 55^2 = 3025
;; Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 - 385 = 2640.
;;
;; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
(range 1 101)

(reduce + (map #(Math/pow % 2) (range 1 101)))
;; => 338350.0

(Math/pow (reduce + (range 1 101)) 2)
;; => 2.55025E7

(-
 (Math/pow (reduce + (range 1 101)) 2)
 (reduce + (map #(Math/pow % 2) (range 1 101)))
 )
;; => 2.516415E7
