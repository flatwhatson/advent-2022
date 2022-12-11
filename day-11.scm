(use-modules (ice-9 match)
             (ice-9 peg)
             (ice-9 pretty-print)
             (ice-9 q)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-43))

(define %example "\
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
")

(define (parse-monkeys str)
  (define-peg-pattern digits body
    (+ (range #\0 #\9)))

  (define-peg-pattern id all
    (and (ignore "Monkey ") digits (ignore ":")))

  (define-peg-pattern item all digits)
  (define-peg-pattern item* body
    (and item (? (and (ignore ", ") item*))))

  (define-peg-pattern items all
    (and (ignore "  Starting items: ") item*))

  (define-peg-pattern old all (ignore "old"))
  (define-peg-pattern plus all (ignore "+"))
  (define-peg-pattern times all (ignore "*"))
  (define-peg-pattern number all digits)

  (define-peg-pattern op body (or old plus times number))
  (define-peg-pattern op* body
    (and op (? (and (ignore " ") op*))))

  (define-peg-pattern next all
    (and (ignore "  Operation: new = ") op*))

  (define-peg-pattern test all
    (and (ignore "  Test: divisible by ") digits))

  (define-peg-pattern on-true all
    (and (ignore "    If true: throw to monkey ") digits))

  (define-peg-pattern on-false all
    (and (ignore "    If false: throw to monkey ") digits))

  (define-peg-pattern monkey all
    (and id (ignore "\n")
         items (ignore "\n")
         next (ignore "\n")
         test (ignore "\n")
         on-true (ignore "\n")
         on-false (ignore "\n")))

  (define-peg-pattern monkeys body
    (and monkey (? (and (ignore "\n") monkeys))))

  (keyword-flatten '(monkey)
                   (peg:tree (match-pattern monkeys str))))

(define-record-type <monkey>
  (%make-monkey id items next test on-true on-false)
  monkey?
  (id monkey-id)
  (items monkey-items)
  (next monkey-next)
  (test monkey-test)
  (on-true monkey-on-true)
  (on-false monkey-on-false))

(define (build-item-queue items)
  (let ((queue (make-q)))
    (for-each (lambda (item)
                (enq! queue (string->number (cadr item))))
              items)
    queue))

(define (build-next-func next)
  (define build-arg
    (match-lambda
      ('old values)
      (('number num)
       (const (string->number num)))))
  (define build-op
    (match-lambda
      ('plus +)
      ('times *)))
  (match next
    ((a op b)
     (let ((a (build-arg a))
           (b (build-arg b))
           (op (build-op op)))
       (lambda (x)
         (op (a x) (b x)))))))

(define make-monkey
  (match-lambda
    (('monkey ('id . id)
              ('items . items)
              ('next . next)
              ('test . test)
              ('on-true . on-true)
              ('on-false . on-false))
     (%make-monkey (string->number (car id))
                   (build-item-queue (keyword-flatten '(item) items))
                   (build-next-func (keyword-flatten '(number) next))
                   (string->number (car test))
                   (string->number (car on-true))
                   (string->number (car on-false))))))

(define (build-monkeys mdata)
  (vector-unfold (lambda (i mdata)
                   (values (make-monkey (car mdata))
                           (cdr mdata)))
                 (length mdata) mdata))

(define (monkey-round-fold proc init monkeys)
  (vector-fold (lambda (ix result m)
                 (let loop ((items (monkey-items m)) (result result))
                   (if (q-empty? items)
                       result
                       (let* ((orig (deq! items))
                              (item ((monkey-next m) orig))
                              (item (quotient item 3))
                              (next-ix (if (zero? (remainder item (monkey-test m)))
                                           (monkey-on-true m)
                                           (monkey-on-false m))))
                         (enq! (monkey-items (vector-ref monkeys next-ix)) item)
                         (loop items (proc ix orig item next-ix))))))
               init monkeys))

(define (count-inspections rounds monkeys)
  (let* ((size (vector-length monkeys))
         (counts (make-vector size 0)))
    (do ((i 0 (1+ i)))
        ((= i rounds))
      (monkey-round-fold
       (lambda (m . _)
         (vector-set! counts m (1+ (vector-ref counts m))))
       #t monkeys))
    (let ((counts (sort! counts >)))
      (* (vector-ref counts 0)
         (vector-ref counts 1)))))

(define (puzzle-0)
  (count-inspections 20 (build-monkeys
                         (parse-monkeys %example))))
