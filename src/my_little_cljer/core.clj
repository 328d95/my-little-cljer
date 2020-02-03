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

(defn member? [a [head & tail :as l]]
  "Is l a member of a?"
  (cond
    (empty? l) false
    (= a head) true
    :else (member? a tail)))

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

(defn lat? [[l & ls :as ll]]
  "Does l only contain atoms?"
  (cond
    (empty? ll) true
    (coll? l) false
    :else (lat? ls)))

; Chapter 7 - Friends and Relations

(defn my-set? [[l & ls :as ll]]
  "Is ll a set?"
  (cond 
    (empty? ll) true
    (member? l ls) false
    :else (my-set? ls)))

(defn makeset [[l & ls :as ll]]
  "Make ll into a set."
  (cond
    (empty? ll) '()
    (my-set? ll) ll
    :else (cons l (makeset (multirember l ls)))))

(defn subset? [[l & ls :as l1] l2]
  "Is l1 a subset of l2?"
  (cond
    (empty? l1) true
    (> (count l1) (count l2)) false
    :else (and (member? l l2) (subset? ls l2))))

(defn eqset? [[s & ss :as set1] set2]
  "Is set1 equal to set2?"
    (and
     (subset? set1 set2)
     (subset? set2 set1)))

(defn intersect? [[s & ss :as set1] set2]
  "Is at least one atom in set1 in set2?"
  (if (empty? set1) false
    (or (member? s set2) (intersect? ss set2))))

(defn intersect [[s & ss :as set1] set2]
  "What are the elements of set1 that are also in set2?"
  (cond
    (empty? set1) '()
    (member? s set2) (cons s (intersect ss set2))
    :else (intersect ss set2)))

(defn union [[s & ss :as set1] set2]
  "What is the set that contains all of set1 and all of set2?"
  (cond
    (empty? set1) set2
    (member? s set2) (union ss set2)
    :else (cons s (union ss set2))))

(defn intersectall [[s & ss :as lset]]
  "What are the common elements in the sets in lset?"
  (cond
    (empty? lset) '()
    (empty? ss) s
    :else (intersect s (intersectall ss))))

(defn a-pair? [l]
  "Does l contain two items?"
  (and (coll? l) (= (count l) 2)))

; (defn first)
; already exists in clojure

; (defn second)
; already exists in clojure

(defn build [s1 s2]
  "Create a list from s1 and s2."
  (cons s1 (cons s2 '())))

(defn third [[l & ls]]
  (second ls))

(defn fun? [rel]
  "Are the first elements of rel a set?"
  (my-set? (firsts rel)))

(defn revpair [pair]
  "Reverse the elements in a pair."
  (build (second pair) (first pair)))

(defn revrel [[l & ls :as rel]]
  "Reverse the sublists in a list."
  (cond
    (empty? rel) '()
    (coll? l) (cons (revpair l) (revrel ls))
    :else (cons l (revrel ls))))

(defn fullfun? [rel]
  "Are the second elements of rel a set?"
  (my-set? (map second rel)))

; Chapter 8 - Lambda the Ultimate

(defn rember-f [f]
  (fn [a [l & ls :as lat]]
    "Calls (rember a l) using f as the comparator function."
    (cond
      (empty? lat) '()
      (f l a) ls
      :else (cons l ((rember-f f) a ls)))))

(defn eq?-c [a]
  (partial = a))

(def eq?-salad (eq?-c "salad"))

(def rember-eq? (rember-f =))

(defn insertL-f [test?]
  "Inserts new before old in lat given a certain equality test."
  (fn [new old [l & ls :as lat]]
    (cond
      (empty? lat) '()
      (test? l old) (cons new lat)
      :else (cons l ((insertL-f test?) new old ls)))))

(defn insertR-f [test?]
  "Inserts new after old in lat."
  (fn [new old [l & ls :as lat]]
    (cond
      (empty? lat) '()
      (test? l old) (cons l (cons new ls))
      :else (cons l ((insertR-f test?) new old ls)))))

(defn seqL [new l ls] )

(defn seqR [new l ls] )

(defn insert-g [cons-order]
  "Insert new left or right of old."
  (fn [new old [l & ls :as lat]]
    (cond 
      (empty? lat) '()
      (= l old) (cons-order new l ls)
      :else (cons l ((insert-g cons-order) new old ls)))))

(def insertL (insert-g (fn [new l ls] (cons new (cons l ls)))))

(def insertR (insert-g (fn [new l ls] (cons l (cons new ls)))))

(def subst (insert-g (fn [new l ls] (cons new ls))))

(defn atom-to-function [x] 
  (cond 
    (= x 'add) add
    (= x 'multi) multi
    :else pow))

; Recur on the subparts that are of the same nature:
; - On the sublists of a list.
; - On the subexpressions of an arithmetic expression. 

(defn value [nexp]
  "What is the value of the solution of nexp?"
  (cond
    (atom? nexp) (if (number? nexp) nexp nil)
    :else ((atom-to-function (second nexp))
           (value (first nexp))
           (value (first-sub-exp nexp)))))

(defn multirember-f [test?]
  (fn [a lat]
    (filter (partial (not (test? a))) lat)))

(def multirember-eq (multirember-f =))

(def eq-tuna? (eq?-c "tuna"))

(defn multiremberT [test? lat]
  (filter #((not (test? %1))) lat))

(defn multirember&co
  [a [l & ls :as lat] col]
  (cond
    (empty? lat) (col '() '())
    (= l a) (multirember&co a ls (fn [newlat seen]
                                   (col newlat (cons l seen))))
    :else (multirember&co a ls (fn [newlat seen]
                                 (col (cons l newlat) seen)))))

(comment
  Execution of multirember&co

  1 -
  tuna
  (strawb tuna and sword)
  (n, s) -> (length n)

  2 -
  tuna
  (tuna and sword)
  (n, s) -> (length (cons strawb n))

  3 -
  tuna
  (and sword)
  (n, s) -> (length (cons strawb n))
  
  4 - 
  tuna
  (sword)
  (n, s) -> (length (cons and (cons strawb n)))

  5 -
  tuna
  ()
  (n, s) -> (length (cons sword (cons and (cons strawb n))))

  ((), ()) -> 3
  )

(defn a-friend [x y] (empty? y))

(defn new-friend [newlat seen]
  (a-friend newlat (cons "tuna" seen)))

(defn last-friend [x y] (length x))

(defn multiinsertL [new old [l & ls :as lat]]
  (cond
    (empty? lat) '()
    (= l old) (cons new (cons old (multiinsertL new old ls)))
    :else (cons l (multiinsertL new old ls))))

(defn multiinsertR [new old [l & ls :as lat]]
  (cond
    (empty? lat) '()
    (= l old) (cons old (cons new (multiinsertR new old ls)))
    :else (cons l (multiinsertR new old ls))))

(defn multiinsertLR [new oldL oldR [l & ls :as lat]]
  (cond 
    (empty? lat) '()
    (= l oldL) (cons new (cons oldL (multiinsertLR new oldL oldR ls)))
    (= l oldR) (cons oldR (cons new (multiinsertLR new oldL oldR ls)))
    :else (cons l (multiinsertLR new oldL oldR ls))))

(defn multiinsertLR&co [new oldL oldR [l & ls :as lat] col]
  (cond
    (empty? lat) (col '() 0 0)
    (= l oldL) 
    (multiinsertLR&co new oldL oldR ls
                      (fn [newlat lIns rIns]
                        (col (cons new (cons oldL newlat)) (inc lIns) rIns)))
    (= l oldR) 
    (multiinsertLR&co new oldL oldR ls
                      (fn [newlat lIns rIns]
                        (col (cons oldR (cons new newlat)) lIns (inc rIns))))
    :else
    (multiinsertLR&co new oldL oldR ls
                      (fn [newlat lIns rIns]
                        (col (cons l newlat) lIns rIns)))))

(defn evens-only* [[l & ls :as lat]]
  "Removes all odd numbers from a list of nested lists."
  (cond
    (empty? lat) '()
    (coll? l) (cons (evens-only* l) (evens-only* ls))
    (even? l) (cons l (evens-only* ls))
    :else (evens-only* ls)))

(defn evens-only*&co [[l & ls :as lat] col]
  "Collects the even numbers, their sum and their product then runs col on the result."
  (cond
    (empty? lat) 
    (col '() 1 0)
    (coll? l) 
    (evens-only*&co 
     l
     (fn [al ap as]
       (evens-only*&co ls
                       (fn [dl dp ds]
                         (col (cons al dl)
                              (* ap dp)
                              (+ as ds))))))
    (even? l)
    (evens-only*&co ls (fn [newl p s]
                         (col (cons l newl)
                              (* l p) s)))
    :else
    (evens-only*&co ls (fn [newl p s]
                         (col newl p (+ l s))))))

(defn keep-looking [a next lat]
  (cond
    (number? next) (keep-looking a (nth lat (- next 1)) lat)
    :else (= a next)))

(defn looking [a lat]
  "Search for a using l as the next index to search, starting with 1."
  (keep-looking a (first lat) lat))

(comment 
  keep-looking is called a partial function because it does not recur on a part of lat.
  
  If the list that is passed to keep-looking contains only numbers then keep-looking will not complete.

  functions that recur on the lists passed to them are called total functions.)

(defn eternity
  "This function is an infinite recursion and therefore the most partial function possible."
  [x]
  (eternity x))

(defn shift 
  "Takes a pair whose first component is a pair and builds a pair by shifting the second part of the first component into the second component."
  [x]
  (build (first (first x))
         (build (second (first x))
                (second x))))

(defn align [pora]
  (cond 
    (not (coll? pora)) pora
    (a-pair? (first pora)) (align (shift pora))
    :else (build (first pora)
                 (align (second pora)))))

(defn length* [pora]
  (cond
    (not (coll? pora)) 1
    :else (+ (length* (first pora))
             (length* (second pora)))))

(defn weight* [pora]
  (cond
    (atom? pora) 1
    :else (+ (* (weight* (first pora)) 2)
             (weight* (second pora)))))

; This function is not total because if you pass a pair of pairs, it fails to complete.
(defn my-shuffle [pora]
  (cond
    (not (coll? pora)) pora
    (a-pair? (first pora)) (my-shuffle (revpair pora))
    :else (build (first pora)
                 (my-shuffle (second pora)))))


; Is this function total?
; It doesn't yield a value for 0, but otherwise
; nobody knows. Thank you, Lothar Collatz (1910 - 1990)
(defn C [n]
  (cond
    (= 1 n) 1
    :else (if (even? n)
            (C (/ n 2))
            (C (inc (* 3 n))))))

(defn A [n m]
  (cond
    (= 0 n) (inc m)
    (= 0 m) (A (dec n) 1)
    :else (A (dec n)
             (A n (dec m)))))

(comment
  (defn will-stop? [f] ...)
  It is impossible to define will-stop? as if we call it on 
  
  (defn last-try [x]
    (and (will-stop? last-try)
         (eternity x)))

  the value of will-stop? is undefined.

  We took a really close look at the two possible cases. If we can define will-stop?, then
  (will-stop? last-try)
  must yield either true or false. But it cannot--due to the very definition of what will-stop? is supposed todo. This must mean that will-stop cannot be defined.
  
  This is the only function with this property.)

(def length1or0 ((fn [mk-length]
                  (mk-length mk-length))
                (fn [mk-length]
                  (fn [l]
                    (cond
                      (empty? l) 0
                      :else (inc
                             ((mk-length eternity)
                              (rest l))))))))

(def mk-length ((fn [mk-length]
                   (mk-length mk-length))
                 (fn [mk-length]
                   (fn [l]
                     (cond
                       (empty? l) 0
                       :else (inc
                              ((mk-length mk-length)
                               (rest l))))))))

(def applicator (fn [x]
                  ((applicator applicator) x)))


(defn fib
  ([n] (fib n 0 1))
  ([n a b]
   (cond
     (= n 1) a
     (= n 2) b
     :else (fib (dec n) b (+ a b)))))


(def almost-factorial
  (fn [f]
    (fn [n]
      (cond
        (= n 0) 1
        :else (* n (f (- n 1)))))))

; normal order Y combinator
; this doesn't work in clojure as it
; has strict execution
(def lazy-Y-combinator
  (fn [f]
    ((fn [x] (x x))
     (fn [x]
       (f (x x))))))

; this is equivalent to the lazy-Y-combinator
(def normal-order-Y-combinator
  (fn [f]
    ((fn [x] (f (x x))) (fn [x] (f (x x))))))


(defn part-factorial [self]
  (let [f (fn [y] ((self self) y))]
    (fn [n]
      (if (= n 0)
        1
        (* n (f (- n 1)))))))


; applicative-order Y combinator
(def Y (fn [non-recursive-fn]
         ; apply x to x
          ; where x is the non-recursive-fn 
          ; wrapped in a lambda which accepts
          ; a function which it wraps in a lambda
          ; and passes to itself.
          ((fn [x] (x x))
           (fn [f]
             (non-recursive-fn (fn [y] ((f f) y)))))))

(def Y-equivalent
  (fn [f]
    ((fn [x] (f (fn [y] ((x x) y))))
     (fn [x] (f (fn [y] ((x x) y)))))))

(def new-entry build)

(defn lookup-in-entry-clj [name entry entry-f]
  (nth (second entry) 
       (.indexOf (first entry) name) 
       (entry-f name)))

(defn lookup-in-entry-help
  [name [na & nas :as names] [va & vas] entry-f]
  (cond
    (empty? names) (entry-f name)
    (= name na) va
    :else (lookup-in-entry-help name nas vas entry-f)))

(defn lookup-in-entry [name entry entry-f]
  (lookup-in-entry-help name
                        (first entry)
                        (second entry)
                        entry-f))

(def extend-table cons)

(defn lookup-in-table [name [t & ts :as table] table-f]
  (cond
    (empty? table) (table-f name)
    :else (lookup-in-entry 
           name 
           t
           (fn [name]
             (lookup-in-table name ts table-f)))))

(defn *const [e table]
  (cond
    (number? e) e
    (= e true) true
    (= e false) false
    :else (build (quote primitive) e)))

(def text-of second)

(defn *quote [e table]
  (text-of e))

(defn initial-table [name]
  (first (quote ())))

(defn *identifier [e table]
  (lookup-in-table e table initial-table))

(defn *fn [e table]
  (build (quote non-primitive)
         (cons table (rest e))))

(def table-of first)
(def formals-of second)
(def body-of third)

(defn else? [x]
  (cond
    (atom? x) (= x (quote else))
    :else false))

(def question-of first)
(def answer-of second)

(defn evcon [[l & ls] table]
  (cond
    (else? (question-of l)) (meaning (answer-of l) table)
    (meaning (question-of l) table) (meaning (answer-of l) table)
    :else (evcon ls table)))

(def cond-lines-of rest)

(defn *cond [e table]
  (evcon (cond-lines-of e) table))

(defn atom-to-action [e]
  (cond
    (number? e) *const
    (= e true) *const
    (= e false) *const
    (= e 'cons) *const
    (= e 'first) *const
    (= e 'rest) *const
    (= e 'empty?) *const
    (= e '=) *const
    (= e 'atom?) *const
    (= e 'zero?) *const
    (= e 'add1) *const
    (= e 'sub1) *const
    (= e 'number?) *const
    :else *identifier))

(defn list-to-action [[e]]
  (cond
    (atom? e)
    (cond
      (= e (quote quote)) *quote
      (= e 'fn) *fn
      (= e 'cond) *cond
      :else *application)
    :else *application))

(defn expression-to-action [e]
  (cond
    (atom? e) (atom-to-action e)
    :else (list-to-action e)))

(defn meaning [e table]
  ((expression-to-action e) e table))

(defn value [e]
  (meaning e '()))

(comment the code from my little schemer doesn't work in
         clojure because the compiler runs down the filter
         and checks that each reference can be found before
         it is used. the meaning, expression-to-action, list-to-action
         functions are co-dependent.)