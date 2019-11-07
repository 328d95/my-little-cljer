(ns my-little-cljer.core)

(defn rember
  "Removes the first instance of a from lat."
  [a [l & ls :as lat]]
  (cond
    (empty? lat) '()
    (= l a) ls
    :else (cons l (rember a ls))))

(defn firsts
  "Take the first item from each list in a list of lists."
  [[l & ls :as all]]
  (cond
    (empty? all) '()
    :else (cons (first l) (firsts ls))))

(defn firsts-clj [l] (map first l))

(defn insertR
  "Inserts new after old in lat."
  [new old [l & ls :as lat]]
  (cond
    (empty? lat) '()
    (= old l) (cons l (cons new ls))
    :else (cons l (insertR new old ls))))

(defn insertL
  "Inserts new before old in lat."
  [new old [l & ls :as lat]]
  (cond
    (empty? lat) '()
    (= old l) (cons new lat)
    :else (cons l (insertL new old ls))))

(defn subst
  "Replaces the first instance of old with new in lat."
  [new old [l & ls :as lat]]
  (cond
    (empty? lat) '()
    (= l old) (cons new ls)
    :else (cons l (subst new old ls))))

(defn subst2
  "What is lat with the first instance of o1 or o2 relaced by new?"
  [new o1 o2 [l & ls :as lat]]
  (cond
    (empty? lat) '()
    (or (= l o1) (= l o2)) (cons new ls)
    :else (cons l (subst2 new o1 o2 ls))))

(defn multirember [a [l & ls :as lat]]
  "What is lat with all instances of a removed?"
  (cond
    (empty? lat) '()
    (= a l) (multirember a ls)
    :else (cons l (multirember a ls))))

(defn multirember-clj [a lat]
  "What is lat with all instances of a removed?"
  (filter #(not (= a %)) lat))

; Numbers
(defn add [n m]
  "What is n plus m?"
  (cond
    (zero? m) n
    :else (add (inc n) (dec m))))

(defn sub [n m]
  "What is n less m?"
  (cond
    (zero? m) n
    :else (sub (dec n) (dec m))))

(defn addtup [[t & up :as tup]]
  "What is the sum of the numbers in tup?"
  (cond
    (empty? tup) 0
    :else (add t (addtup up))))

(defn multi [n m]
  "What is n multiplied by m?"
  (cond
    (zero? n) 0
    :else (add m (multi (dec n) m))))

(defn tup+ [[t1 & ts1 :as tup1] [t2 & ts2 :as tup2]]
  "What is the tuple that is the sum of t1 and t2?"
  (cond
    (empty? tup1) tup2
    (empty? tup2) tup1
    :else (cons (add t1 t2) (tup+ ts1 ts2))))

(defn gt [n m]
  "Is n greater than m?"
  (cond
    (zero? n) false
    (zero? m) true
    :else (gt (dec n) (dec m))))

(defn lt [n m]
  "Is n less than m?"
  (cond 
    (zero? m) false
    (zero? n) true
    :else (lt (dec n) (dec m))))

(defn eq [n m]
  "Are n and m equal?"
  (cond
    (gt n m) false
    (lt n m) false
    :else true))

(defn pow [n m]
  "What is n to the power of m?"
  (cond
    (zero? m) 1
    :else (multi n (pow n (dec m)))))

(defn div [n m]
  "How many times does m fit into n?"
  (cond
    (lt n m) 0
    :else (inc (div (sub n m) m))))

(defn length [lat]
  "How long is lat?"
  (cond
    (empty? lat) 0
    :else (inc (length (rest lat)))))

(defn pick [n [l & at :as lat]]
  "What is the nth element of lat?"
  (cond
    (zero? n) nil
    (zero? (dec n)) l
    :else (pick (dec n) at)))

(defn rempick [n [l & at :as lat]]
  "What is lat with the nth element removed?"
  (cond
    (empty? lat) '()
    (zero? (dec n)) at
    :else (cons l (rempick (dec n) at))))

(defn no-nums [[l & at :as lat]]
  "What is lat with all of the number elements removed?"
  (cond
    (empty? lat) '()
    (number? l) (no-nums at)
    :else (cons l (no-nums at))))

(defn all-nums [[l & at :as lat]]
  "What are the numbers contained in lat?"
  (cond
    (empty? lat) '()
    (number? l) (cons l (all-nums at))
    :else (all-nums at)))

; atom? like in scheme does not exist in clojure.
; Clojure has a different concept for atoms:
; Reference types that can be updated atomically
; -- used for global state and concurrent state.
(defn atom? [x]
  (not (coll? x)))

(defn eqan? [a1 a2]
  "Are a1 and a2 the same atom?"
  (cond
    ; == is for numbers in clojure
    (and (number? a1) (number? a2)) (== a1 a2)
    ; = is for everything in clojure but given
    ; that we have made two checks, this
    ; will function similar to a scheme version
    ; of this function.
    (and (atom? a1) (atom? a2)) (= a1 a2)
    :else false))

(defn occur [a [l & at :as lat]]
  "How many times does a occur in lat?"
  (cond
    (empty? lat) 0
    (= a l) (inc (occur a at))
    :else (occur a at)))

(defn one? [n]
  "Is n one?"
  (= n 1))

; Re-written with one?
(defn rempick2 [n [l & at :as lat]]
  "What is lat with the nth element removed?"
  (cond
    (empty? lat) '()
    (one? n) at
    :else (cons l (rempick2 (dec n) at))))