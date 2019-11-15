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
    ; that we have two checks, this
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

(defn rember* [n [head & tail :as l]]
  "What is l with n removed from it and its S-expressions?"
  (cond
    (empty? l) '()
    (atom? head) 
    (if (= n head) 
      (rember* n tail)
      (cons head (rember* n tail)))
    :else (cons (rember* n head) (rember* n tail))))

(defn insertR* [new old [head & tail :as l]]
  "What is l with new inserted to the right of every old?"
  (cond
    (empty? l) '()
    (atom? head) 
    (if (= old head)
      (cons head (cons new (insertR* new old tail)))
      (cons head (insertR* new old tail)))
    :else (cons (insertR* new old head) (insertR* new old tail))))

(defn occur* [a [head & tail :as l]]
  "How many times does a occur in l, where l is a list of lists?"
  (cond
    (empty? l) 0
    (atom? head)
    (if (= a head)
      (inc (occur* a tail))
      (occur* a tail))
    :else (add (occur* a head) (occur* a tail))))

(defn subst* [new old [head & tail :as l]]
  "What is l when old is replaced by new?"
  (cond
    (empty? l) '()
    (atom? head)
    (if (= old head)
      (cons new (subst* new old tail))
      (cons head (subst* new old tail)))
    :else (cons (subst* new old head) (subst* new old tail))))

(defn insertL* [new old [head & tail :as l]]
  "What is l when new is inserted before each old?"
  (cond
    (empty? l) '()
    (atom? head)
    (if (= old head)
      (cons new (cons head (insertL* new old tail)))
      (cons head (insertL* new old tail)))
    :else (cons (insertL* new old head) (insertL* new old tail))))

(defn member* [a [head & tail :as l]]
  "Is l a member of a?"
  (cond
    (empty? l) false
    (= a head) true
    :else
     (if (coll? head)
       (or (member* a head) (member* a tail))
       (member* a tail))))

; this is not a * function because
; it only recurs on head and not tail.
(defn leftmost [[head & tail :as l]]
  "What is the first atom in l?"
  (cond
    (empty? l) nil
    (atom? head) head
    :else (leftmost head)))

(defn eqlist? [[head1 & tail1 :as l1] [head2 & tail2 :as l2]]
  "Are the elements in l1 and l2 all the same?"
  (cond
    (and (empty? l1) (empty? l2)) true
    (eqan? head1 head2) (eqlist? tail1 tail2)
    (or (atom? head1) (atom? head2)) false
    :else (and
           (eqlist? head1 head2)
           (eqlist? tail1 tail2))))

(defn equal? [s1 s2]
  "Are s1 and s2 equal?"
  (cond
    (eqan? s1 s2) true
    (or (atom? s1) (atom? s2)) false 
    :else (eqlist? s1 s2)))

(defn first-sub-exp [aexp] (nth aexp 2))
(defn second-sub-exp [aexp] (nth aexp 3))

(defn numbered? [aexp]
  "Does this arithmetic expression only contain numbers?"
    (cond
      (atom? aexp) (number? aexp)
      :else (and
       (numbered? (first aexp)) 
       (numbered? (first-sub-exp aexp)))))

; Recur on the subparts that are of the same nature:
; - On the sublists of a list.
; - On the subexpressions of an arithmetic expression. 

(defn value [nexp]
  "What is the value of the solution of nexp?"
  (cond
    (atom? nexp) (if (number? nexp) nexp nil)
    (= (second nexp) 'add) (add (value (first nexp)) (value (first-sub-exp nexp)))
    (= (second nexp) 'multi) (multi (value (first nexp)) (value (first-sub-exp nexp)))
    :else (pow (value (first nexp)) (value (first-sub-exp nexp)))))

; Use help functions to abstract from representations.

; If we represent numbers as: 1 - (()), 2 - ((), ())

(defn sero? [n]
  "Is n zero?"
  (empty? n))

(defn edd1 [n]
  "What is n + 1?"
  (cons '() n))

(defn zub1 [n]
  "What is n - 1?"
  (rest n))

(defn edd [n m]
  "What is n + m?"
  (cond
    (empty? n) m
    :else (edd (zub1 n) (edd1 m))))

(defn lat? [[ll & ls :as l]]
  "Does l only contain atoms?"
  (cond
    (empty? l) true
    (coll? ll) false
    :else (lat? ls)))

