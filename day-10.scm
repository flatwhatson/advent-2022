(use-modules (ice-9 match)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9))

(define %example-short "\
noop
addx 3
addx -5
")

(define %example-long "\
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
")

(define (parse-program str)
  (filter-map (lambda (line)
                (match (string-split line #\space)
                  (("") #f)
                  ((cmd) (string->symbol cmd))
                  ((cmd arg) (cons (string->symbol cmd)
                                   (string->number arg)))))
              (string-split str #\newline)))

(define (fold-program proc init program)
  (let loop ((program program) (cycle 1) (regx 1) (result init))
    (match program
      (()
       (proc cycle regx result))
      (('noop . program)
       (let ((result (proc cycle regx result)))
         (loop program (1+ cycle) regx result)))
      ((('addx . n) . program)
       (let* ((result (proc cycle regx result))
              (result (proc (1+ cycle) regx result)))
         (loop program (+ cycle 2) (+ regx n) result))))))

(define (sum-strengths start step program)
  (fold-program (lambda (cycle regx sum)
                  (if (= (modulo cycle step) start)
                      (+ (* cycle regx) sum)
                      sum))
                0 program))

(define (puzzle-0)
  (sum-strengths 20 40 (parse-program %example-long)))
