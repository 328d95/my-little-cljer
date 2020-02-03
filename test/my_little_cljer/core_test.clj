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

(deftest numbered?-test
  (testing "numbered?"
    (is (= (numbered? '(3 add (4 pow 5))) true))
    (is (= (numbered? 1) true))
    (is (= (numbered? '(2 multi "sausage")) false))))

(deftest value-test
  (testing "value"
    (is (= (value 13) 13))
    (is (= (value '(1 add 3)) 4))
    (is (= (value '(1 add (3 pow 4))) 82))
    (is (= (value "cookie") nil))))

(deftest sero?-test
  (testing "sero?"
    (is (= (sero? '(())) false))
    (is (= (sero? '()) true))))

(deftest edd1-test
  (testing "edd1"
    (is (= (edd1 '()) '(())))
    (is (= (edd1 '(())) '(() ())))))

(deftest zub1-test
  (testing "zub1"
    (is (= (zub1 '()) '()))
    (is (= (zub1 '(())) '()))))

(deftest edd-test
  (testing "edd"
    (is (= (edd '() '()) '()))
    (is (= (edd '() '(())) '(())))
    (is (= (edd '(()) '(() ())) '(() () ())))))

(deftest lat?-test
  (testing "lat?"
    (is (= (lat? '()) true))
    (is (= (lat? '(())) false))
    (is (= (lat? '("bacon")) true))
    (is (= (lat? '(1 2 3 4)) true))
    (is (= (lat? '([] [] [])) false))))

; Chapter 7 - Friends and Relations

(deftest my-set?-test
  (testing "my-set?"
    (is (= (my-set? '("apples" "peaches" "pears" "plums")) true))
    (is (= (my-set? '()) true))))

(deftest member?-test
  (testing "member?"
    (is (= (member? 1 '(1)) true))
    (is (= (member? 1 '()) false))
    (is (= (member? 1 '(2 3)) false))
    (is (= (member? 1 '(2 3 1)) true))))

(deftest makeset-test
  (testing "makeset"
    (is (= (makeset '("apple" "peach" "pear" "peach" "plum" "apple" "lemon" "peach")) 
           '("apple" "peach" "pear" "plum" "lemon")))))

(deftest subset?-test
  (testing "subset?"
    (is (= (subset? 
            '(5 "chicken" "wings")
            '(5 "hamburgers" 2 "pieces" "fried" "chicken" "and" "light" "duckling" "wings"))
           true))
    (is (= (subset?
            '(4 "pounds" "of" "horseradish")
            '("four" "pounds" "chicken" "and" 5 "ounces" "horseradish"))
           false))))

(deftest eqset?-test
  (testing "eqset?"
    (is (= (eqset?
            '(6 "large" "chickens" "with" "wings")
            '(6 "chickens" "with" "large" "wings"))
           true))))

(deftest intersect?-test
  (testing "intersect?"
    (is (= (intersect?
            '("stewed" "tomatoes" "and" "macaroni")
            '("macaroni" "and" "cheese"))
           true))))

(deftest intersect-test
  (testing "intersect"
    (is (= (intersect
            '("stewed" "tomatoes" "and" "macaroni")
            '("macaroni" "and" "cheese"))
           '("and" "macaroni")))))

(deftest union-test
  (testing "union"
    (is (= (union
            '("stewed" "tomatoes" "and" "macaroni" "casserole")
            '("macaroni" "and" "cheese"))
           '("stewed" "tomatoes" "casserole" "macaroni" "and" "cheese")))))

(deftest intersectall-test
  (testing "intersectall"
    (is (= (intersectall
            '((1 2 3) (3 1 4 5) (5 6 7 8 1 2)))
           '(1)))
    (is (= (intersectall
            '((6 "pears" "and")
              (3 "peaches" "and" 6 "peppers")
              (8 "pears" "and" 6 "plums")
              ("and" 6 "prunes" "with" "some" "apples")))
           '(6 "and")))))

(deftest a-pair-test
  (testing "a-pair?"
    (is (= (a-pair? '("pear" "pear")) true))
    (is (= (a-pair? '(3 7)) true))
    (is (= (a-pair? '((2) ("pair"))) true))
    (is (= (a-pair? '("full" ("house"))) true))))

(deftest build-test
  (testing "build"
    (is (= (build 1 2) '(1 2)))
    (is (= (build 1 '()) '(1 ())))))

(deftest third-test
  (testing "third"
    (is (= (third '(1 2 3)) 3))
    (is (= (third '(1 2 4 5)) 4))))

; (deftest rel-test
;   (testing "rel?"
;     (is (= (rel? '("apples" "peaches" "pumpkin" "pie")) 
;            false))
;     (is (= (rel? '(("apples" "peaches")
;                    ("pumpkin" "pie")
;                    ("apples" "peaches")))
;            false))
;     (is (= (rel? '(("apples" "peaches")
;                    ("pumpkin" "pie")))
;            true))
;     (is (= (rel? '((4 3) (4 2) (7 6) (6 2) (3 4)))
;            true))))

(deftest fun?-test
  (testing "fun?"
    (is (= (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))) true))
    (is (= (fun? '(("d" 4) ("b" 0) ("b" 9) ("e" 5) ("g" 4))) false))))

(deftest revrel-test
  (testing "revrel"
    (is (= (revrel '((8 "a") ("pumpkin" "pie") ("got" "sick")))
           '(("a" 8) ("pie" "pumpkin") ("sick" "got"))))
    (is (= (revrel '((1 2) (3 4) (5 6)))
           '((2 1) (4 3) (6 5))))))

(deftest fullfun?-test
  (testing "fullfun?"
    (is (= (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))
           false))
    (is (= (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4)))
           true))
    (is (= (fullfun? '(("grape" "raisin") ("plum" "prune") ("stewed" "prune")))
           false))
    (is (= (fullfun? '(("grape" "raisin") ("plum" "prune") ("stewed" "grape")))
           true))))

(deftest rember-f-test
  (testing "rember-f"
    (is (= ((rember-f =) 5 '(6 2 5 3))
           '(6 2 3)))
    (is (= ((rember-f =) "jelly" '("jelly" "beans" "are" "good"))
           '("beans" "are" "good")))
    (is (= ((rember-f =) '("pop" "corn") '("lemonade" ("pop" "corn") "and" ("cake")))
           '("lemonade" "and" ("cake"))))
    (is (= ((rember-f =) "tuna" '("shrimp" "salad" "and" "tuna" "salad"))
           '("shrimp" "salad" "and" "salad")))))

(deftest eq?-salad-test
  (testing "eq?-salad"
    (is (= (eq?-salad "salad") true))
    (is (= (eq?-salad "turkey") false))))

(deftest rember-eq?-test
  (testing "rember-eq?"
    (is (= (rember-eq? "tuna" '("tuna" "salad" "is" "good"))
           '("salad" "is" "good")))))

(deftest atom-to-function-test
  (testing "atom-to-function"
    (is (= (atom-to-function (first '(add 5 3)))
           add))))

(deftest multiremberT-tuna-test
  (testing "multiremberT"
    (is (= (multiremberT eq-tuna? '("shrimp" "salad" "tuna" "salad" "and" "tuna"))))))

(deftest multirember&co-test
  (testing "multirember&co"
    (is (= (multirember&co "tuna" '("and" "tuna") a-friend)
           false))
    (is (= (multirember&co "tuna" '("strawberries" "tuna" "and" "swordfish") last-friend)
           3))))

(deftest multiinsertLR&co-test
  (testing "multiinsertLR&co"
    (is (= (multiinsertLR&co "salty" "fish" "chips" 
                             '("chips" "and" "fish" "or" "fish" "and" "chips")
                             (fn [newlat L R]
                               newlat))
           '("chips" "salty" "and" "salty" "fish" "or" "salty" "fish" "and" "chips" "salty")))))

(deftest evens-only*-test
  (testing "evens-only*"
    (is (= (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))
           '((2 8) 10 (() 6) 2)))))

(deftest evens-only*&co-test
  (testing "evens-only*&co"
    (is (= (evens-only*&co 
            '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
            (fn [newl product sum]
              (cons sum (cons product newl))))
           '(38 1920 (2 8) 10 (() 6) 2)))))

(deftest looking-test
  (testing "looking"
    (is (= (looking "caviar" '(6 2 4 "caviar" 5 7 3))
           true))
    (is (= (looking "caviar" '(6 2 "grits" "caviar" 5 7 3))
           false))))

(deftest shift-test
  (testing "shift"
    (is (= (shift '((:a :b) :c))
           '(:a (:b :c))))
    (is (= (shift '((:a :b) (:c :d)))
           '(:a (:b (:c :d)))))))

(deftest weight*-test
  (testing "weight*"
    (is (= (weight* '((:a :b) :c))
           7))
    (is (= (weight* '(:a (:b :c)))
           5))))

(deftest shuffle-test
  (testing "my-shuffle"
    (is (= (my-shuffle '(:a (:b :c)))
           '(:a (:b :c))))
    (is (= (my-shuffle '(:a :b)) '(:a :b)))))

(deftest add-test
  (testing "A"
    (is (= (A 1 0) 2))
    (is (= (A 1 1) 3))
    (is (= (A 2 2) 7))))

(deftest length1or0-test
  (testing "length1or0"
    (is (= (length1or0 '(1)) 1))
    (is (= (length1or0 '()) 0))))

(deftest mk-length-test
  (testing "mk-length"
    (is (= (mk-length '(:apple)) 1))
    (is (= (mk-length '(1 2)) 2))))

(deftest lookup-in-entry-test
  (testing "lookup-in-entry"
    (is (= (lookup-in-entry :entree 
                            '((:appetizer :entree :beverage)
                              (:food :tastes :good))
                            (fn [x] x))
           :tastes))))

(deftest lookup-in-table-test
  (testing "lookup-in-table"
    (is (= (lookup-in-table
            :entree
            '(((:entree :dessert)
               (:spaghetti :spumoni))
              ((:appetizer :entree :beverage)
               (:food :tastes :good)))
            (fn [x] x)))
        :spaghetti)))

(deftest *cond-test
  (testing "*cond"
    (is (= (*cond 
            '(cond ('coffee 'klatsch) ('else 'party))
            '((('coffee) (true))
              ('klatsch 'party) (5 (6))))))))