;1.
(define-struct hurricane (name category  maximum-sustained-winds velocity heading))

;; a hurricane is a (make-hurricane String Natural Natural Natural String)
;; interp:  represents a hurricane constrictor where
;; name is name of the hurricane
;; its category (a number between 1 and 5, inclusive)
;; maximum-sustained-winds is the maximum sustained winds in miles per hour
;; velocity is the velocity of the storm in miles per hour
;; the heading is  the storm's heading (for example, NNW)

(define KATRINA (make-hurricane "Katrina" 5 175 50 "NW"))

(define-struct thunderstorm (inches-of-rainfall maximum-wind-gust velocity heading))

;;a thunderstorm is a (make-thunderstorm Natural Natural Natural String)
;; interp:  represents a thunderstorm constrictor where
;; inches-of-rainfall is he number of inches of rainfall of the thunderstorm
;; maximum-wind-gust is the maximum wind gust in miles per hour 
;; velocity is the velocity of the storm in miles per hour
;; the heading is  the storm's heading (for example, NNW)

(define CLAP-OF-THUNDER (make-thunderstorm 5 40 40 "S"))

(define-struct fire (square-miles number-of-days people-displaced))

;;a fire is a (make-fire Natural Natural Natural)
;; interp:  represents a fire constrictor where
;;  square-miles  representes by the number of square miles it the fire covers
;;  number-of-days is the number of days it has been raging
;;  people-displaced is the number of people displaced by the fire

(define DA-FIRE (make-fire 100 10 3000))


; A storm is one of
; hurricane
; thunderstorm
; fire





;2.
; (define(hurricane-fcn a-hurricane)
; ...(hurricane-name a-hurricane)
;    (hurricane-category a-hurricane)
;    (hurricane-maximum-sustained-winds a-hurricane)
;    (hurricane-velocity a-hurricane)
;    (hurricane-heading a-hurricane))
; 


; (define(thunderstorm-fcn a-thunderstorm)
; ...(thunderstorm-inches-of-rainfall a-thunderstorm)
;    (thunderstorm-maximum-wind-gust a-thunderstorm)
;    (thunderstorm-velocity a-thunderstorm)
;    (thunderstorm-heading a-thunderstorm)


; (define(fire-fcn a-fire)
; ...(fire-square-miles a-fire)
;    (fire-number-of-days a-fire)
;    (fire-people-displaced a-fire)


; NEED STORM TEMPLATE



(define ONELIST (cons "Neo" empty))
(define TWOLIST (cons "The Captain" (cons "Tennille" empty)))
(define THREELIST (cons "onion" (cons "celery" (cons "bell pepper" empty))))

;3

;;high-impact? : a-storm -> boolean
;consumes a storm and produces a boolean. The function returns true if the storm is a category 4 or 5 hurricane,a thunderstorm with more than 3 inches of rainfall and winds exceeding 60mph, or a fire covering at least 50 square miles.

(check-expect (high-impact? KATRINA) #true)
(check-expect (high-impact? (make-hurricane "Arnold" 4 145 25 "N")) #true)
(check-expect (high-impact? (make-hurricane "Joe" 2 85 15 "W")) #false)
(check-expect (high-impact? (make-thunderstorm 2 30 40 "E")) #false)
(check-expect (high-impact? (make-thunderstorm 3 40 40 "NW")) #false)
(check-expect (high-impact? CLAP-OF-THUNDER) #false)
(check-expect (high-impact? (make-thunderstorm 2 100 85 "SW")) #false)
(check-expect (high-impact? (make-thunderstorm 3 100 5 "S")) #false)
(check-expect (high-impact? (make-thunderstorm 5 100 34 "NE")) #true)
(check-expect (high-impact? DA-FIRE) #true)
(check-expect (high-impact? (make-fire 50 4 1000)) #true)
(check-expect (high-impact? (make-fire 40 1 150)) #false)

(define (high-impact? a-storm)
  (cond [(hurricane? a-storm)  (if (= (hurricane-category a-storm) 4)
                               #true
                               (if (= (hurricane-category  a-storm) 5)
                               #true
                               #false))]
                           
        [(thunderstorm? a-storm)  (if (> (thunderstorm-inches-of-rainfall a-storm) 3)
                                  (if (> (thunderstorm-maximum-wind-gust  a-storm) 60)
                                  #true
                                  #false)
                                  #false)]
                              
        [(fire? a-storm)  (if (>= (fire-square-miles  a-storm) 50)
                           #true
                           #false )]))

;4

;;change-heading : a-storm heading -> a-storm
;consumes a storm and a heading and produces a storm. The storm is returned unchanged if the given storm is a fire. Otherwise, the storm that's produced is a storm the same as the original, except that the heading has been changed to the given heading. 

(check-expect (change-heading KATRINA "S") (make-hurricane "Katrina" 5 175 50 "S"))
(check-expect (change-heading KATRINA "NW") (make-hurricane "Katrina" 5 175 50 "NW"))
(check-expect (change-heading CLAP-OF-THUNDER "E") (make-thunderstorm 5 40 40 "E"))
(check-expect (change-heading CLAP-OF-THUNDER "S") (make-thunderstorm 5 40 40 "S"))
(check-expect (change-heading DA-FIRE "S") (make-fire 100 10 3000))
(check-expect (change-heading DA-FIRE "NW") (make-fire 100 10 3000))

(define (change-heading a-storm new-heading)

   (cond [(hurricane? a-storm) (make-hurricane (hurricane-name a-storm) (hurricane-category a-storm) (hurricane-maximum-sustained-winds a-storm) (hurricane-velocity a-storm) new-heading)]
         [(thunderstorm? a-storm) (make-thunderstorm (thunderstorm-inches-of-rainfall a-storm) (thunderstorm-maximum-wind-gust a-storm) (thunderstorm-velocity a-storm) new-heading)]                             
         [(fire? a-storm) (make-fire (fire-square-miles a-storm) (fire-number-of-days a-storm) (fire-people-displaced a-storm))]))

;5

;; character-count : ListOfString -> Natural
;  consumes a ListOfString and counts the total number of characters in all strings in the list.

(check-expect (character-count empty) 0)
(check-expect (character-count ONELIST) 3)
(check-expect (character-count TWOLIST) 19)
(check-expect (character-count THREELIST)22)
(check-expect (character-count (cons "War" (cons "Famine" (cons "Pestilence" (cons "Death" empty)))))24)


(define (character-count alist) 
   (cond [(empty? alist)  0 ]
         [(cons? alist)  (+ (string-length (first alist))(character-count (rest alist)))]))

;6

;; numeric-strings : ListOfString -> ListOfString
;  consumes a ListOfString and produces a ListOfString that contains only those strings from the original list that consist entirely of numeric characters

;(numeric-strings (cons "123" (cons "abc" (cons "456" (cons "789" empty)))))

(define (numeric-strings list)
  (cond [(empty? (first list)) list]
        [(string-numeric? (first list)) (numeric-strings (cons (first list) (rest list)))]
        [else (cons (rest list))]))
        
