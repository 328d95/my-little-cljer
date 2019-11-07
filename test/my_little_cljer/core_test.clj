(ns my-little-cljer.core-test
  (:require [clojure.test :refer :all]
            [my-little-cljer.core :refer :all]))

(deftest firsts-test
  (testing "firsts"
    (is (= (firsts '(("a" "b") ("c" "d") ("e" "f"))) '("a" "c" "e")))))

(deftest rember-test
  (testing "rember"
    (is (= 
         (rember "and" '("bacon" "lettuce" "and" "tomato"))
         '("bacon" "lettuce" "tomato")))))

(deftest firsts-clj-test
  (testing "firsts-clj"
    (is (= (firsts-clj '(("a" "b") ("c" "d") ("e" "f"))) '("a" "c" "e")))))

(deftest insertR-test
  (testing "insertR"
    (is (= (insertR
            "topping" "fudge"
            '("ice" "cream" "fudge" "for" "dessert"))
           '("ice" "cream" "fudge" "topping" "for" "dessert")))
    (is (= (insertR
            "jalapeno" "and"
            '("tacos" "tamales" "and" "salsa"))
           '("tacos" "tamales" "and" "jalapeno" "salsa")))))

(deftest insertL-test
  (testing "insertL"
    (is (= (insertL
            "a" "b"
            '("b" "c" "d" ))
           '("a" "b" "c" "d")))
    (is (= (insertL
            "c" "d"
            '("a" "b" "d"))
            '("a" "b" "c" "d")))
    (is (= (insertL
            "jalapeno" "salsa"
            '("tacos" "tamales" "and" "salsa"))
           '("tacos" "tamales" "and" "jalapeno" "salsa")))))

(deftest subst-test
  (testing "subst"
    (is (= (subst
            "topping" "fudge"
            '("ice" "cream" "with" "fudge" "for" "dessert")))
        '("ice" "cream" "with" "topping" "for" "dessert"))))

(deftest subst2-test
  (testing "subst2"
    (is (= (subst2
            "vanilla"
            "chocolate"
            "banana"
            '("banana" "ice" "cream" "with" "chocolate" "topping"))
           '("vanilla" "ice" "cream" "with" "chocolate" "topping")))))

(deftest multirember-test
  (testing "multirember"
    (is (= (multirember
            1
            '(1 2 1 3 1 4 5))
           '(2 3 4 5)))))

(deftest multirember-clj-test
  (testing "multirember-clj"
    (is (= (multirember-clj
            1
            '(1 2 1 3 1 4 5))
           '(2 3 4 5)))))

(deftest add-test
  (testing "add"
    (is (= (add 5 5) 10))
    (is (= (add 0 5) 5))
    (is (= (add 123 5) 128))
    (is (= (add 5 500) 505))))

(deftest add-test
  (testing "add"
    (is (= (add 5 5) 10))
    (is (= (add 0 5) 5))
    (is (= (add 123 5) 128))
    (is (= (add 5 500) 505))))

(deftest addtup-test
  (testing "addtup"
    (is (= (addtup '(1 2 3 4 5)) 15))
    (is (= (addtup '(1 1 1 1)) 4))
    (is (= (addtup '(2 2 2 2)) 8))))

(deftest multi-test
  (testing "multi"
    (is (= (multi 1 5) 5))
    (is (= (multi 2 123) 246))
    (is (= (multi 5 5) 25))))

(deftest tup+-test
  (testing "tup+"
    (is (= (tup+ '(1 1) '(1 1)) '(2 2)))
    (is (= (tup+ '(1 2 3) '(1 2 3 4)) '(2 4 6 4)))))

(deftest gt-test
  (testing "gt"
    (is (= (gt 0 0) false))
    (is (= (gt 1 0) true))
    (is (= (gt 0 1) false))))

(deftest lt-test
  (testing "lt"
    (is (= (lt 0 0) false))
    (is (= (lt 1 0) false))
    (is (= (lt 0 1) true))))

(deftest eq-test
  (testing "eq"
    (is (= (eq 0 0) true))
    (is (= (eq 1 0) false))
    (is (= (eq 0 1) false))
    (is (= (eq 100 100) true))))

(deftest pow-test
  (testing "pow"
    (is (= (pow 0 1) 0))
    (is (= (pow 1 1) 1))
    (is (= (pow 2 3) 8))
    (is (= (pow 5 3) 125))))

(deftest div-test
  (testing "div"
    (is (= (div 15 4) 3))
    (is (= (div 15 5) 3))
    (is (= (div 1 13) 0))
    (is (= (div 0 12) 0))))

(deftest length-test
  (testing "length"
    (is (= (length '(1 2 3 4 5)) 5))
    (is (= (length '()) 0))
    (is (= (length '(1 2 3)) 3))))

(deftest pick-test
  (testing "pick"
    (is (= (pick 0 '(1 2 3)) nil))
    (is (= (pick 4 '(1 2 3)) nil))
    (is (= (pick 1 '(1 2 3)) 1))
    (is (= (pick 1 '()) nil))))

(deftest rempick-test
  (testing "rempick"
    (is (= (rempick 0 '(1 2 3)) '(1 2 3)))
    (is (= (rempick 1 '(1 2 3)) '(2 3)))
    (is (= (rempick 4 '(1 2 3)) '(1 2 3)))
    (is (= (rempick 1 '()) '()))))

(deftest no-nums-test
  (testing "no-nums"
    (is (= (no-nums '(1 2 3)) '()))
    (is (= (no-nums '(5 "pears" 6 "prunes" 9 "dates")) '("pears" "prunes" "dates")))
    (is (= (no-nums '("a" "b" "c")) '("a" "b" "c")))
    (is (= (no-nums '()) '()))))

(deftest all-nums-test
  (testing "all-nums"
    (is (= (all-nums '(1 2 3)) '(1 2 3)))
    (is (= (all-nums '(5 "pears" 6 "prunes" 9 "dates")) '(5 6 9)))
    (is (= (all-nums '("a" "b" "c")) '()))
    (is (= (all-nums '()) '()))))

(deftest eqan?-test
  (testing "eqan?"
    (is (= (eqan? '() 2) false))
    (is (= (eqan? '() '()) false))
    (is (= (eqan? 2 '()) false))
    (is (= (eqan? 2 2) true))
    (is (= (eqan? "a" 2) false))
    (is (= (eqan? 2 "a") false))
    (is (= (eqan? "a" "a") true))
    (is (= (eqan? "a" '()) false))
    (is (= (eqan? '() "a") false))))

(deftest occur-test
  (testing "occur"
    (is (= (occur 1 '(1 2 3 4)) 1))
    (is (= (occur 1 '()) 0))
    (is (= (occur 1 '(2 3 4))))
    (is (= (occur 2 '(2 2 2 2)) 4))))

(deftest one?-test
  (testing "one?"
    (is (= (one? 1) true))
    (is (= (one? 2) false))
    (is (= (one? '()) false))))

(deftest rempick2-test
  (testing "rempick2"
    (is (= (rempick2 0 '(1 2 3)) '(1 2 3)))
    (is (= (rempick2 1 '(1 2 3)) '(2 3)))
    (is (= (rempick2 4 '(1 2 3)) '(1 2 3)))
    (is (= (rempick2 1 '()) '()))))

(deftest rember*-test
  (testing "rember*"
    (is (= 
         (rember* 
          "cup" 
          '(("coffee") "cup" (("tea") "cup") ("and" ("hick")) "cup")) 
         '(("coffee") (("tea")) ("and" ("hick")))))
    (is (= 
         (rember* 
          "sauce"
          '((("tomato" "sauce")) (("bean") "sauce") ("and" (("flying") "sauce")))) 
         '((("tomato")) (("bean")) ("and" (("flying"))))))))

(deftest insertR*-test
  (testing "insertR*"
    (is (=
         (insertR*
          "roast"
          "chuck"
          '(("how" "much" ("wood"))
            "could"
            (("a" ("wood") "chuck"))
            ((("chuck")))
            ("if" ("a") (("wood" "chuck")))
            "could" "chuck" "wood")))
         '(("how" "much" ("wood"))
           "could"
           (("a" ("wood") "chuck" "roast"))
           ((("chuck" "roast")))
           ("if" ("a") (("wood" "chuck" "roast")))
           "could" "chuck" "roast" "wood"))))

(deftest occur*-test
  (testing "occur*"
    (is (=
         (occur* 
          "banana"
          '(("banana")
           ("split" (((("banana" "ice")))
                     ("cream" ("banana"))
                     "sherbet"))
           ("banana")
           ("bread")
           ("banana" "brandy")))
         5))))

(deftest subst*-test
  (testing "subst*"
    (is (=
         (subst* 
          "orange"
          "banana"
          '(("banana")
             ("split" (((("banana" "ice")))
                       ("cream" ("banana"))
                       "sherbet"))
             ("banana")
             ("bread")
             ("banana" "brandy")))
         '(("orange")
           ("split" (((("orange" "ice")))
                     ("cream" ("orange"))
                     "sherbet"))
           ("orange")
           ("bread")
           ("orange" "brandy"))))))

(deftest insertL*-test
  (testing "insertL*"
    (is (=
         (insertL*
          "pecker"
          "chuck"
          '(("how" "much" ("wood"))
            "could"
            (("a" ("wood") "chuck"))
            ((("chuck")))
            ("if" ("a") (("wood" "chuck")))
            "could" "chuck" "wood"))
         '(("how" "much" ("wood"))
           "could"
           (("a" ("wood") "pecker" "chuck"))
           ((("pecker" "chuck")))
           ("if" ("a") (("wood" "pecker" "chuck")))
           "could" "pecker" "chuck" "wood")
           ))))

(deftest member*-test
  (testing "member*"
    (is (=
         (member*
          "chips"
          '(("potato") ("chips" (("with") "fish") ("chips")))
          )
         true))))

(deftest leftmost-test
  (testing "leftmost"
    (is (= (leftmost '(("potato") ("chips" (("with") "fish") ("chips"))))
           "potato"))
    (is (= (leftmost '((("hot") ("tuna" ("and"))) "cheese"))
           "hot"))
    (is (= (leftmost '(((() "four")) 17 ("seventeen")))
           nil))))

(deftest eqlist?-test
  (testing "eqlist?"
    (is (= (eqlist? '("strawberry" "ice" "cream")
                    '("strawberry" "ice" "cream"))
           true))
    (is (= (eqlist? '("strawberry" "ice" "cream")
                    '("strawberry" "cream" "ice"))
           false))
    (is (= (eqlist? '("banana" (("split")))
                    '(("banana") ("split")))
           false))
    (is (= (eqlist? '("beef" (("sausage")) ("and" ("soda")))
                    '("beef" (("salami")) ("and" ("soda"))))
           false))
    (is (= (eqlist? '("beef" (("sausage")) ("and" ("soda")))
                    '("beef" (("sausage")) ("and" ("soda"))))
           true))))

(deftest equal?-test
  (testing "equal?"
    (is (= (equal? '(1 2 3) '(1 2 3)) true))
    (is (= (equal? '() '()) true))
    (is (= (equal? '() 1) false))
    (is (= (equal? 1 1) true))
    (is (= (equal? 1 2) false))))