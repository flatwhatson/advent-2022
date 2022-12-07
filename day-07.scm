(use-modules (ice-9 match)
             (ice-9 peg)
             (ice-9 pretty-print)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-9))

(define %example "\
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
")

;;; Input parser

(define-peg-pattern filename* body
  (+ (or (range #\a #\z) ".")))

(define-peg-pattern filename all
  filename*)

(define-peg-pattern filesize all
  (+ (range #\0 #\9)))

(define-peg-pattern command-cd all
  (and (ignore "$ cd ") (or "/" ".." filename*) (ignore "\n")))

(define-peg-pattern command-ls all
  (ignore "$ ls\n"))

(define-peg-pattern output-dir all
  (and (ignore "dir ") filename (ignore "\n")))

(define-peg-pattern output-file all
  (and filesize (ignore " ") filename (ignore "\n")))

(define-peg-pattern terminal-output body
  (* (or command-cd command-ls output-dir output-file)))

(define (parse-terminal-output str)
  (peg:tree (match-pattern terminal-output str)))

;;; Filesystem structures

(define-record-type <dir>
  (%make-dir name size entries)
  dir?
  (name dir-name)
  (size dir-size set-dir-size!)
  (entries dir-entries set-dir-entries!))

(define-record-type <file>
  (make-file name size)
  file?
  (name file-name)
  (size file-size))

(define (make-dir name)
  (%make-dir name 0 '()))

(define (dir-or-file-name entry)
  ((if (dir? entry) dir-name file-name) entry))

(define (dir-or-file-size entry)
  ((if (dir? entry) dir-size file-size) entry))

(define (add-dir-entry! dir entry)
  (set-dir-entries! dir (cons entry (dir-entries dir)))
  (set-dir-size! dir (+ (dir-or-file-size entry) (dir-size dir))))

(define* (print-file file #:optional (indent ""))
  (format #t "~a- ~a (~a)\n" indent (file-name file) (file-size file)))

(define* (print-dir dir #:optional (indent ""))
  (format #t "~a+ ~a (~a)\n" indent (dir-name dir) (dir-size dir))
  (for-each (lambda (entry)
              ((if (dir? entry) print-dir print-file)
               entry (string-append indent "  ")))
            (sort (dir-entries dir)
                  (lambda (a b)
                    (string<? (dir-or-file-name a)
                              (dir-or-file-name b))))))

;; Command interpreter

(define (build-filesystem-tree cmds)
  (let loop ((cmds cmds) (path '()))
    (match cmds
      (() (match path
            ((root) root)
            ((dir . _)
             ;; pretend we got "cd .." up to root
             (loop '((command-cd "..")) path))))
      ((('command-cd "..") . cmds)
       ;; pop directory from path, add to parent
       (let* ((dir (car path))
              (path (cdr path))
              (parent (car path)))
         (add-dir-entry! parent dir)
         (loop cmds path)))
      ((('command-cd name) . cmds)
       ;; push directory onto path
       (let* ((dir (make-dir name))
              (path (cons dir path)))
         (loop cmds path)))
      (('command-ls . cmds)
       (loop cmds path))
      ((('output-dir _) . cmds)
       (loop cmds path))
      ((('output-file ('filesize size) ('filename name)) . cmds)
       ;; add file to current directory
       (let ((file (make-file name (string->number size)))
             (parent (car path)))
         (add-dir-entry! parent file)
         (loop cmds path))))))

(define (filesystem-tree-fold proc init tree)
  (let loop ((items (list tree)) (result init))
    (match items
      (() result)
      ((item . rest)
       (loop (if (dir? item)
                 (append (dir-entries item) rest)
                 rest)
             (proc item result))))))

(define (filesystem-tree-filter pred tree)
  (filesystem-tree-fold (lambda (item result)
                          (if (pred item)
                              (cons item result)
                              result))
                        '() tree))

;;; Solutions

(define (find-dirs-with-size cmp size tree)
  (filesystem-tree-filter (lambda (item)
                            (and (dir? item)
                                 (cmp (dir-size item) size)))
                          tree))

(define (puzzle-0)
  (let* ((commands (parse-terminal-output %example))
         (tree (build-filesystem-tree commands))
         (dirs (find-dirs-with-size <= 100000 tree)))
    (apply + (map dir-size dirs))))

(define (puzzle-1)
  (let* ((input (call-with-input-file "data/input-07" get-string-all))
         (commands (parse-terminal-output input))
         (tree (build-filesystem-tree commands))
         (dirs (find-dirs-with-size <= 100000 tree)))
    (apply + (map dir-size dirs))))

(define (puzzle-2)
  (let* ((input (call-with-input-file "data/input-07" get-string-all))
         (commands (parse-terminal-output input))
         (tree (build-filesystem-tree commands))
         (unused (- 70000000 (dir-size tree)))
         (wanted (- 30000000 unused))
         (dirs (find-dirs-with-size >= wanted tree)))
    (apply min (map dir-size dirs))))
