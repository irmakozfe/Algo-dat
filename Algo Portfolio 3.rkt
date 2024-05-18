#lang racket
;aufgabe 1
(define (anpassen liste)
  (if (empty? liste)
      (list)
      (if (odd? (car liste))
          (anpassen (cdr liste))
          (if (=(remainder (car liste) 10)0)
              (append (list (sqr (car liste))) (anpassen (cdr liste)))
              (append (list (car liste)) (anpassen (cdr liste)))))))
(anpassen (list 5 9 10 12 20))

;aufgabe 2
(define (gleich? liste)
(if (= 0 (iter liste)) 
    #t
    #f)
  )
  
  (define (iter liste)
    (cond
      [(empty? liste) 0]
      [(< 0 (car liste))
        (+ 1 (iter (cdr liste)))]
        [ (< (car liste) 0)
          (-  (iter (cdr liste)) 1)]
        (else (cdr liste))))
  (gleich? '( 1 3 4 -2 3 -5 -6 -7))

;aufgabe 3
(define (sortieren liste praedikat)
  (define (iter liste praedikat sort rest)
    (if (empty? liste)
        (append sort rest)
        (if (praedikat (car liste))
            (iter (cdr liste) praedikat (append  sort (list (car liste))) rest)
            (iter (cdr liste) praedikat sort (append  rest (list (car liste)))))))
  (iter liste praedikat '() '()))

;alternative 
(define (sort liste praedikat)
  (group-by praedikat liste))

(define (sortieren liste praedikat)
 (flatten (sort liste praedikat)) )
(sortieren '(1 2 3 4 5 6 7 8 9) odd?)


(sortieren '(1 2 3 4 5 6 7 8 9) odd?)



                