#lang racket

(require racket/string)

(provide interpret)

;------------------------------------------------------------------------------
; Parsing constants
;------------------------------------------------------------------------------

; Sections dividers
(define personae "Dramatis personae")
(define settings "Settings")
(define finis "Finis")

; Comment lines
(define comments '("Act" "Scene"))

; List of all "bad words" in a definition
(define bad-words
  '("vile"
    "villainous"
    "wicked"
    "naughty"
    "blackhearted"
    "shameless"
    "scoundrelous"))

; Arithmetic
(define add "join'd with")
(define mult "entranc'd by")

; Self-reference keywords
(define self-refs
  '("I"
    "me"
    "Me"
    "myself"
    "Myself"))

; Function call
(define call "The song of")

; Function parameter name
(define param "Hamlet")

;------------------------------------------------------------------------------
; Interpreter driver
;------------------------------------------------------------------------------

#|
(interpret filename)
  filename: a string representing the path to a FunShake file

  Returns a list of numbers produced when evaluating the FunShake file.
|#
(define (interpret filename)
  (let* ([contents (port->string (open-input-file filename))]
         [lines (map normalize-line (string-split contents "\n"))]
         ; Ignore title, empty, and comment lines
         [body (remove-empty-and-comments (rest lines))])
    (evaluate body)))

#|
(normalize-line str)
  str: the line string to normalize

  Remove trailing period and whitespace.
|#
(define (normalize-line str)
  (string-trim (string-normalize-spaces (string-trim str)) "."))

#|
(remove-empty-and-comments strings)
  strings: a list of strings

  Removes all empty strings and FunShake comment strings from 'strings'.
|#
(define (remove-empty-and-comments strings)
  (filter (lambda (s)
            (and
             (< 0 (string-length s))
             (not (ormap (lambda (comment) (prefix? comment s))
                         comments))))
          strings))

#|
(prefix? s1 s2)
  s1, s2: strings

  Returns whether 's1' is a prefix of 's2'.
|#
(define (prefix? s1 s2)
  (and (<= (string-length s1) (string-length s2))
       (equal? s1 (substring s2 0 (string-length s1)))))

;------------------------------------------------------------------------------
; Main evaluation
;------------------------------------------------------------------------------

#|
(evaluate body)
  body: a list of lines corresponding to the semantically meaningful text
  of a FunShake file.

  Returns a list of numbers produced when evaluating the FunShake file.
|#
(define (evaluate body)
  (let*  ([char-lines (first (split-sections body))]
          [settings-lines (second (split-sections body))]
          [dialogue-lines (third (split-sections body))]
          [char-vals (get-bindings eval-character char-lines)]
          [settings-vals (get-bindings eval-setting settings-lines)]
          [dialogue-pairs (parse-dialogue dialogue-lines)])
    (map (lambda(pair) (eval-dialogue (cdr pair) (car pair) char-vals settings-vals)) dialogue-pairs)))

#|
(split-sections body)
  body: a list of strings where each string is a line in the FunShake file

  Returns a list of 3 lists: a list of the character lines, a list of the settings-lines
  (could be empty), and a list of the dialogue lines.

|#
(define (split-sections body)
  (let* ([char-lines (get-char-lst body)]
         [settings-and-more (drop body (+ 1 (sublist (list finis) body)))]
         [settings-index (sublist (list finis) settings-and-more)]
         [settings-lines (if (not (equal? settings-index #f))
                             (take (rest settings-and-more) (- settings-index 1))
                             '() )]
         [dialogue-lines (if (not (equal? settings-index #f))
                             (drop settings-and-more (+ 1 settings-index ))
                             settings-and-more)])
    (list char-lines settings-lines dialogue-lines)))

;------Dialogue section functions------

#|
(parse-dialogue lines)
  lines: a list of strings where each string is a line in the dialogue section

  Returns a list of pairs where first element of each pair is the name of the person saying the line
  and the second element is the line that they are saying.

|#
(define (parse-dialogue lines)
  (if (empty? lines)
      '()
      (append (list (cons (string-trim (first lines) ":") (second lines))) (parse-dialogue (drop lines 2)))))

#|
(eval-dialogue line speaker char-vals settings-vals)
  line: a string representing a line of dialogue
  speaker: the name of the character who spoke line
  char-vals: the list of character name-value binding pairs 
  settings-vals: the list of setting name-value binding pairs

  Returns the value of the line spoken by speaker.

|#
(define (eval-dialogue line speaker char-vals settings-vals)
  ; split the dialogue line
  ; get the first 3 words in the line
  (let* ([split (string-split line)]
         [first-3 (if (> (length split) 3)
                      (take split 3)
                      '())])
    ; if the first 3 words match 'call' (i.e. it's a function call), call eval-function-call
    ; if not, call eval-string
    (if (equal? first-3 (string-split call))
        (let* ([func-name (fourth split)]
               [expr (string-join (drop split 5))])
          (eval-function-call func-name expr speaker char-vals settings-vals))
        (eval-string line speaker char-vals))))

#|
(eval-expression expr speaker char-vals)
  expr: a list of strings representing part of a line
  speaker: the name of the character who spoke line
  char-vals: the list of character name-value binding pairs 

  Returns the value of the expression.

|#
(define (eval-expression expr speaker char-vals)
  (if (equal? (length expr) 1)
      (name-lookup (first expr) speaker char-vals)
      (eval-description expr)))

#|
(eval-arithmetic expr1 expr2 op speaker char-vals)
  expr1: a list of strings representing the first argument in an arithmetic expression
  expr2: a list of strings representing the second argument in an arithmetic expression
  op: the arithmetic operator, either + or *
  speaker: the name of the character who spoke line
  char-vals: the list of character name-value binding pairs 

  Returns the value of performing the arithmetic operator on expr1 and expr2.

|#
(define (eval-arithmetic expr1 expr2 op speaker char-vals)
  (op (eval-expression expr1 speaker char-vals) (eval-expression expr2 speaker char-vals)))

#|
(eval-string str speaker char-vals)
  str: a string which is either an arithmetic expression, a name lookup, or a description
  speaker: the name of the character who spoke line
  char-vals: the list of character name-value binding pairs 

  Returns the value of str.

|#
(define (eval-string str speaker char-vals)
  (let* ([split (string-split str)])
    (if (equal? (length split) 1)
        (name-lookup str speaker char-vals)
        ; case: this is not a name lookup. is either arithmetic expr or regular description
        (let* ([add-pos (sublist (string-split add) split)]
               [mult-pos (sublist (string-split mult) split)])
          (if (equal? add-pos #f)
              (if (equal? mult-pos #f)
                  ; this is just a regular description
                  (eval-description split)
                  ; arithmetic: multiply
                  (eval-arithmetic (take split mult-pos) (drop split (+ mult-pos 2)) * speaker char-vals))
              ; arithmetic: add
              (eval-arithmetic (take split add-pos) (drop split (+ add-pos 2)) + speaker char-vals))))))


#|
(name-lookup str speaker char-vals)
  str: a string which represents part of a line
  speaker: the name of the character who spoke line
  char-vals: the list of character name-value binding pairs 

  Returns the value of str/speaker in char-vals, if it's there, or the value of the word str
  if it's not there. 

|#
(define (name-lookup str speaker char-vals)
  (if (contains-word? self-refs str)
      ; used one self-refs keyword only. then return the speaker's value
      (get-val speaker char-vals)
      ; else check if they said a character's name. if yes, return that char's val, else it's just a word
      (let* ([char-val (get-val str char-vals)])
        (if (not (equal? #f char-val))
            char-val
            (eval-description (list str))))))



#|
(sublist-start sub lst)
  sub: a list
  lst: a list

  Checks whether sub is a sublist of lst starting at the first element of lst.
  Returns #t if it is, and #f if not.

|#
(define (sublist-start sub lst)
  (if (empty? sub)
      #t
      (if (empty? lst)
          #f
          (let* ([res (sublist-start (rest sub) (rest lst))])
            (and (equal? (first sub) (first lst)) res)))))

#|
(sublist sub lst)
  sub: a list
  lst: a list

  Checks whether 'sub' is a sublist of 'lst' (i.e., all the items in
  'sub' appear consecutively in 'lst').

  If 'sub' is a sublist of 'lst', this function returns the *index*
  of the first element of the first occurrence of 'sub' within 'lst'.
  Otherwise, this function returns #f.

  Note that the empty list is a sublist of every list, and it first
  occurs at index 0.

> (sublist '(30 40) '(10 20 30 40 50))
2
> (sublist '(20 30) '(10 20 30 20 30 40 50))
1
> (sublist '(1 2 3) '(5 4 3 2 1))
#f
|#
(define (sublist sub lst)
  (if (empty? lst)
      #f
      (if (sublist-start sub lst)
          0
          (let* ([res (sublist sub (rest lst))])
            (if (equal? res #f)
                #f
                (+ 1 (sublist sub (rest lst))))))))

; -------Settings section functions-----------
#|
(get-bindings eval-func lines)
  eval-func: the function used to evaluate the line (either eval-setting or eval-character)
  lines: a list of strings where each string is a line in the settings section

  Returns a list of pairs where the first element of each pair is the name of the setting (function)
  and the second element is the description of the setting (body of the function).

|#
(define (get-bindings eval-func lines)
  (map (lambda(line) (eval-func line)) lines))
#|
(eval-setting setting-line)
  line: a string which is a line in the settings section

  Returns a pair where the first element of is the name of the setting (function)
  and the second element is the description of the setting (body of the function).

|#
(define (eval-setting setting-line)
  (let* ([settings-str-lst (string-split setting-line ", ")]
         [name (first settings-str-lst)]
         [desc (rest settings-str-lst)])
    (cons name (first desc))))

#|
(eval-function-call name expr speaker char-vals settings-vals)
  name: the name of the function to be called
  expr: a list of the strings which is the expression to be evaluated and then passed into the function
  speaker: the name of the character who spoke the line
  char-vals: the list of character name-value binding pairs 
  settings-vals: the list of setting name-value binding pairs

  Returns the value of the line after calling the function name on expr.

|#
(define (eval-function-call name expr speaker char-vals settings-vals)
  (let* ([arg (eval-string expr speaker char-vals)]
         [func-body (get-val name settings-vals)]
         [updated-char-vals (append (filter (lambda(x)
                                              (not (equal? (car x) param)))
                                            char-vals) (list (cons param arg)))])
    (eval-dialogue func-body speaker updated-char-vals settings-vals)))

#|
(get-val name vals-list)
  name: the name of the function to be called
  settings-vals: the list of either character or setting name-value binding pairs

  Returns the second element in the name-value binding pair if name is in the list, otherwise
  returns false. 

|#
(define (get-val name vals-list)
  (let* ([result (filter (lambda (x)
                           (equal? (car x) name))
                         vals-list)])
    (if (empty? result)
        #f
        (cdr (first result)))))

; -------Character section functions---------

#|
(get-char-lst body)
  body: a list of strings where each string is a line in the FunShake file

  Returns the list of strings where each string is a line of the dramatis personae section. 

|#
(define (get-char-lst body)
  (let* ([index (sublist (list finis) (rest body))])
    (take (rest body) index)))

#|
(eval-character char-line)
  char-line: a string which is a line in the characters section

  Takes in a dramatis personae line, and evaluates that characters description
  (by calling eval-description), and outputs a name-value binding (as a pair).

  > (eval-character "MacGries, a scoundrelous and vile merchant")
  ("MacGries" . -20)
|#
(define (eval-character char-line)
  (let* ([char-str-lst (string-split char-line ", ")]
         [name (first char-str-lst)]
         [desc (rest char-str-lst)])
    (if (empty? desc)
        (cons name 0)
        (cons name (eval-description (string-split (first desc)))))))

#|
(eval-description descrip)
  descrip: a list of strings which is a character's description

  Returns the value of descrip, according to the specifications. 

  > (eval-description '("a" "scoundrelous" "and" "vile" "merchant"))
  -20
|#
(define (eval-description descrip)
  (let* ([n (length descrip)]
         [b (count-bad descrip)])
    (if (equal? b 0)
        n
        (* -1 (expt 2 b) n))))

#|
(count-bad lst)
  lst: a list of words

  Returns the number of bad words in lst. 

  > (count-bad '("a" "scoundrelous" "and" "vile" "merchant") 0)
  2
|#

(define (count-bad lst)
  (length (filter (lambda (x)
                           (contains-word? bad-words x))
                         lst)))

#|
(contains-word? str-lst word)
  str-lst: a list of words
  word: a str (the word you are searching for)

  Returns true iff str-lst contains a particular word 'word'.

  > (contains-word? '("a" "scoundrelous" "and" "vile" "merchant") "and")
  #t
|#
(define (contains-word? str-lst word)
  (if (empty? (filter (lambda(x)
                        (equal? x word)) str-lst))
      #f
      #t))