;;;     Parser combinator using functional programming in Scheme.
;;;     Copyright (C) 2019 Carlos Roman Rivera
;;;
;;;     This program is free software: you can redistribute it and/or modify
;;;     it under the terms of the GNU General Public License as published by
;;;     the Free Software Foundation, either version 3 of the License, or
;;;     (at your option) any later version.
;;;
;;;     This program is distributed in the hope that it will be useful,
;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;     GNU General Public License for more details.
;;;
;;;     You should have received a copy of the GNU General Public License
;;;     along with this program.  If not, see <https://www.gnu.org/licenses/>


;;; Function that you pass to someone and let them call it at some point of time.

(define get-callback
  (lambda (combinators)
    (cond ((null? combinators) (lambda (x) x))
	  ((null? (cdr combinators)) (car combinators))
	  (else (error 'get-callback "Callback Error.")))))

;;; Testing and debugging parsers written using parser combinators.
;;; Checks if parser is able to construct an expression from given tokens.
;;; Uses the parser as a function and evaluates tokens.
;;; Returns the tokens that could be parsed successfully.

(define test
  (lambda (parser tokens)
    (parser tokens
	    (lambda (pe tokens)
	      `((expression: ,pe) (tokens left: ,tokens)))
	    (lambda () 'error))))

;;; Recognizes expressions at character level and defines a constant.
;;; Receives a string to match and an optional postprocessor.
;;; If received tokens (s) matches the given string's head, continue.

;;; Note: optional parameters are defined by placing a . before their declaration.

(define const
  (lambda (match? . postprocessor)
    (let ((combinator (get-callback postprocessor)))
      (lambda (s match fail)
      	(cond ((null? s) (fail))
      	      ((match? (car s))
      	       (match (combinator (car s)) (cdr s)))
      	      (else (fail)))))))

;;; (define parser-example (const (lambda (x) (and (number? x) (= x 69)))))
;;; (test parser-example '(1 2))
;;; (test parser-example '(69 1 2))

;;; (define parser-example (const (lambda (x) (and (number? x) (= x 69))) (lambda (_) 'sixty-nine)))
;;; (test parser-example '(1 2))
;;; (test parser-example '(69 1 2))

;;; Takes parsers as arguments and returns their catenation.
;;; Loops through each parser and catenates each parser with the next one in a list.

;;; Note: letrec - recursive binding form that permits the definition of
;;; mutually recursive values that contain procedures.

(define caten
  (letrec ((loop
	    (lambda (parsers)
	      (if (null? parsers)
		  (lambda (s match fail)
		    (match '() s))
		  (let ((parser1 (car parsers))
			(parser2 (loop (cdr parsers))))
		    (lambda (s match fail)
		      (parser1 s
		       (lambda (e s)
			 (parser2 s
			  (lambda (es s)
			    (match (cons e es) s))
			  fail))
		       fail)))))))
    (lambda parsers
      (lambda combinators
	(let ((parser (loop parsers))
	      (combinator (if (null? combinators) list (get-callback combinators))))
	  (lambda (s match fail)
	    (parser s
	     (lambda (es s) (match (apply combinator es) s))
	     fail)))))))

;;; Example using parser catenator.
;;; Define a parser for identifying a single number.
;;; Then catenate two parsers to identify two continuous numbers.

(define number
    (lambda (n)
      (const
       (lambda (x)
         (and (number? x)
              (= x n))))))

;;; (define parser-example ((caten (number 1) (number 2))))
;;; (test parser-example '(3 2 1 4))
;;; (test parser-example '(1 2 3 4))

;;; Takes parsers as arguments and returns their disjunction.
;;; Tries to parse given tokens with first parser.
;;; If it succeeds, ends. If not, go to next parser.
;;; If we run out of parsers, fail.

(define disj
  (letrec
    ((loop
	    (lambda
        (parsers)
        (if (null? parsers)
		      (lambda (s match fail) (fail))
		      (let ((parser1 (car parsers))
      			(parser2 (loop (cdr parsers))))
      		    (lambda (s match fail)
      		      (parser1 s
      		       match
      		       (lambda () (parser2 s match fail)))))))))
    (lambda
      parsers
      (lambda combinators
	      (let ((parser (loop parsers)) (combinator (get-callback combinators)))
	    (lambda (s match fail)
  	    (parser s
  	     (lambda (e s)
  	       (match (combinator e) s))
  	     fail)))))))

;;; (define parser-example ((disj (number 1) (number 3) (number 5))))
;;; (test parser-example '(1 2 3 4 5))
;;; (test parser-example '(1 9))
;;; (test parser-example '(2 8))

;;; Takes parser and optional postprocessor.
;;; Returns parser for grammar that matches list of zero or more strings matched by p.

(define star
  (lambda (parser . combinators)
    (let ((combinator (get-callback combinators)))
      (letrec ((special-star
		(lambda $
		  (apply
		   ((disj
		     ((caten parser special-star) cons)
		     ((caten))))
		   $))))
	((disj special-star) combinator)))))

;;; Variation of Kleene star. Omits the first term of the star union.

(define plus
  (lambda (parser . combinators)
    (let ((combinator (get-callback combinators)))
      ((disj ((caten parser (star parser)) cons))
       combinator))))

;;; (define parser-example (star (number 69)))
;;; (test parser-example '(1 2 3 4 5))
;;; (test parser-example '(69 69 69 69 69 4 5 69 6))

(define aux-char
  (lambda (char=?)
    (lambda (ch)
      (const (lambda (c) (char=? c ch))))))

(define char (aux-char char=?))

;;; Note: char-ci=? compares charactes ignoring case.

(define char-ci (aux-char char-ci=?))

(define aux-word
  (lambda (char=?)
    (lambda (string . combinators)
      (let ((combinator (get-callback combinators)))
	((disj
	  ((apply caten
		  (map (lambda (ch) (const (lambda (c) (char=? ch c))))
		    (string->list string)))
	   (lambda s
	     (combinator (list->string s))))))))))

;;; Note: char=? compares two or more characters

(define word (aux-word char=?))

(define word-ci (aux-word char-ci=?))
