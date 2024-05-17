#lang racket

; blatt 7
(define (loesche liste praedikat)
     (if (praedikat (car liste))
         (loesche (cdr liste) praedikat)
         liste

         ))

(loesche (list 4 6 8 3 2 4 5) even?)

(define (drehe liste)
  (define (iter liste new)
    (if (empty? liste)
        new
(iter (cdr liste) (cons (car liste) new))))
  (iter liste '()))

(drehe (list 1 2 3)) 
(drehe (list 1 2 (list 3))) 
(drehe (list 1 (list 5 6) 2 (list 3 4)))


(define (caesar data key)
  (define (iter data key ykey)
  (if (null? key)
      (iter data ykey ykey)
    (if (empty? data)
        (list)
       (cons (remainder (+ (car data) (car key))10) (iter (cdr data) (cdr key) ykey)))))
  (iter data key key))
  
(caesar
(list 1 2 3 4 5 6) (list 1 3 3 7))

; blatt 8

(define (compress liste)
  (define (iter liste sayac)
    (if (empty? liste)
        (list)
            (cond
              [(=(length liste) 1)
               (if (= sayac 1)
                   (cons (car liste) (iter (cdr liste) sayac))
                   (cons sayac (cons (car liste) (iter (cdr liste) sayac))))]

              [ (= sayac 1)
                (if (equal? (car liste) (cadr liste))
                    (iter (cdr liste) (+ sayac 1))
                    (cons (car liste) (iter (cdr liste) 1)))]
              
              [(> sayac 1)
               (if (equal? (car liste) (cadr liste))
                   (iter (cdr liste) (+ sayac 1))
                   (cons sayac (cons (car liste) (iter (cdr liste) 1))))]
                (else '()))))
            
  (iter liste 1))
               
            (compress '(a b c))
(compress '(a b b c c c))
(compress '(a b b c c c a b c)) 
(compress '(a a a a a a a a a a))


(define (expandiere liste)
  (define (iter liste sayac)
    (if (<= 2 (length liste))
    (if (integer? (car liste))
        (if (< sayac (car liste))
            (cons (cadr liste) (iter liste (+ 1 sayac)))
            (iter (cdr (cdr liste)) 0))
        (cons (car liste) (iter (cdr liste) 0)))
    '()))
  (iter liste 0))
        (expandiere '(7 b a 3 c))


(define (loeschen liste n)
  (if (< 0 n)
      (loeschen (cdr liste) (- n 1))
      liste))
(loeschen '(2 3 4 5 6 7) 3 )

;blatt 9


(define (liste-teilen eingabe)
  (define (iter eingabe l1 l2)
    (if (empty? eingabe)
        (list l1 l2)
   (if (odd? (car eingabe))
       (iter (cdr eingabe) (append l1 (list (car eingabe))) l2)
        (iter (cdr eingabe) l1 (append l2 (list (car eingabe)))))))
    
  (iter eingabe '() '()))
(liste-teilen '(1 2 3 4 5 6 7 8 9))
(liste-teilen '(1 2 3 4 5 6 7 8 9 10))
(liste-teilen '())

(define (l1 n) (car n))
(define (l2 n) (cadr n))

(define (verschmelzen eingabe)
  (iter (l1 eingabe) (l2 eingabe) '()))

  (define (iter l1 l2 new)
    (cond
      [(empty? l1) (if (empty? l2)
                       new
                    (iter l1 (cdr l2) (append new (list (car l2)))))]
      [(empty? l2) (iter (cdr l1) l2 (append new (list (car l1)))) ]
      [(and (empty? l1) (empty? l2)) new ]
      (else (append (list (car l1)) (list (car l2)) new
                    (iter (cdr l1) (cdr l2) new)))))


     
(verschmelzen '((1 3 5 7 9) (2 4 6 8 10)))
(verschmelzen '(() ())) 

(define (hamming ein1 ein2)
  (define (iter ein1 ein2 sayac)
    (if (empty? ein1)
        sayac
        (if (= (car ein1) (car ein2))
            (iter (cdr ein1) (cdr ein2) sayac)
            (iter (cdr ein1) (cdr ein2) (+ sayac 1)))))
  (iter ein1 ein2 0))

(hamming '(1 0 1 1 0 1 0 1) '(0 1 1 1 0 1 0 0)) 
(hamming '(1 0 1) '(1 0 1)) 
                
  



