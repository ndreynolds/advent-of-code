#lang racket
;;
;; Day 2: Inventory Management System
;;
;; Usage: racket day02.rkt < ../inputs/02.txt

(define (read-ids)
  (let ([id (read-line)])
    (if (eof-object? id)
        '()
        (cons id (read-ids)))))

(define (part1 ids)
  (define (frequencies lst)
    (map length (group-by identity lst)))

  (define (duplicates? lst)
    (member 2 (frequencies lst)))

  (define (triplicates? lst)
    (member 3 (frequencies lst)))

  (* (count duplicates? ids)
     (count triplicates? ids)))

(define (part2 ids)
  (define (close-match? x y)
    (and (not (eq? x y))
         (= (- (length x)
                (count check-duplicates (map list x y)))
            1)))

  (define (close-matches lst)
    (filter (lambda (pair) (apply close-match? pair))
            (cartesian-product lst lst)))

  (define (common-elems a b)
    (map car (filter check-duplicates (map list a b))))

  (apply common-elems (car (close-matches ids))))

(let ([ids (map string->list (read-ids))])
  (displayln (part1 ids))
  (displayln (part2 ids)))
