#lang racket
;;
;; Day 1: Chronal Calibration --
;;
;; Usage: racket day01.rkt < ../inputs/01.txt

(define (read-modulations)
  (let ([line (read-line)])
     (if (eof-object? line)
         '()
         (cons (string->number line) (read-modulations)))))

(define (part1 mods)
  (foldl + 0 mods))

(define (part2 mods)
  (define mods-stream (sequence->stream (in-cycle mods)))

  (define (duplicate-freq prev-freqs freq mstream)
    (if (set-member? prev-freqs freq)
        freq
        (duplicate-freq (set-add prev-freqs freq)
                        (+ freq (stream-first mstream))
                        (stream-rest mstream))))

  (duplicate-freq (set) 0 mods-stream))

(let ([mods (read-modulations)])
  (displayln (part1 mods))
  (displayln (part2 mods)))
