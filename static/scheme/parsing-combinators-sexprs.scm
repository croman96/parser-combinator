;;; parsing-combinators-sexprs.scm
;;; Using the parsing combinator package to parse sexprs
;;;
;;; Programmer: Mayer Goldberg, 2013

;;; Replace the following with the full path to your parser-combinator script.

(load "/Users/croman/Workspace/parser-combinator/static/scheme/parser-combinator.scm")

;;; You can use this super cool s-expression to test your parser using:

;;; (read-sexpr "(defun factorial (x) (if (zerop x) 1 (* x (factorial (- x 1)))))")

(define read-sexpr
  (lambda (string)
    (test <sexpr> (string->list string))))

(define read-sexpr*
  (lambda (string)
    (test <sexpr>* (string->list string))))

;;;

(define char
  (lambda (c)
    (const (lambda (ch) (char-ci=? ch c)))))

(define word
  (lambda (string . cbs)
    (let ((cb (get-callback cbs)))
      ((disj
	((apply caten
		(map (lambda (ch) (const (lambda (c) (char-ci=? ch c))))
		  (string->list string)))
	 (lambda s
	   (cb (list->string s)))))))))

(define <boolean>
  ((disj (word "#t" (lambda _ #t))
	 (word "#f" (lambda _ #f)))))

(define <octal-digit>
  (const (lambda (ch) (and (char<=? #\0 ch) (char<=? ch #\7)))
	 (let ((base (char->integer #\0)))
	   (lambda (ch) (- (char->integer ch) base)))))

(define <oct-byte>
  ((caten <octal-digit> <octal-digit> <octal-digit>)
   (lambda (o1 o2 o3)
     (+ o3 (* 8 (+ o2 (* 8 o1)))))))

(define <one-byte-oct-char>
  ((caten (word "#\\o")
	  <oct-byte>)
   (lambda (_ value)
     (integer->char value))))

(define <hex-digit>
  ((disj (const (lambda (ch) (and (char<=? #\0 ch) (char<=? ch #\9)))
		(let ((base (char->integer #\0)))
		  (lambda (ch) (- (char->integer ch) base))))
	 (const (lambda (ch) (and (char-ci<=? #\a ch) (char-ci<=? ch #\f)))
		(let ((base (char->integer #\a)))
		  (lambda (ch) (+ 10 (- (char->integer (char-downcase ch)) base))))))))

(define <hex-byte>
  ((caten <hex-digit> <hex-digit>)
   (lambda (h1 h2)
     (+ h2 (* 16 h1)))))

(define <hex-double-byte>
  ((caten <hex-byte> <hex-byte>)
   (lambda (b1 b2)
     (+ b2 (* 256 b1)))))

(define <two-byte-hex-char>
  ((caten (word "#\\x")
	  <hex-double-byte>)
   (lambda (_ value)
     (integer->char value))))

(define <one-byte-hex-char>
  ((caten (word "#\\x")
	  <hex-byte>)
   (lambda (_ value)
     (integer->char value))))

(define *char-alef* (integer->char 1488))
(define *char-bet* (integer->char 1489))
(define *char-gimel* (integer->char 1490))
(define *char-smiley* (integer->char 9786))
(define *char-pbuh* (integer->char 65018))
(define *char-devanagari-om* (integer->char 2384))
(define *char-tibetan-om* (integer->char 3840))

(define <char>
  ((disj
    (word "#\\space" (lambda (_) #\space))
    (word "#\\newline" (lambda (_) #\newline))
    (word "#\\tab" (lambda (_) #\tab))
    (word "#\\page" (lambda (_) #\page))
    (word "#\\return" (lambda (_) #\return))
    ;; and just to make a point that we can define our own named chars
    (word "#\\alef" (lambda (_) *char-alef*))
    (word "#\\aleph" (lambda (_) *char-alef*))
    (word "#\\bet" (lambda (_) *char-bet*))
    (word "#\\gimel" (lambda (_) *char-gimel*))
    (word "#\\smiley" (lambda (_) *char-smiley*))
    (word "#\\pbuh" (lambda (_) *char-pbuh*))
    (word "#\\om" (lambda (_) *char-devanagari-om*))
    (word "#\\tibetan-om" (lambda (_) *char-tibetan-om*))
    <two-byte-hex-char>
    <one-byte-hex-char>
    <one-byte-oct-char>
    ;;
    ((caten (word "#\\")
	    (const (lambda (ch) #t) (lambda (ch) ch)))
     (lambda (_ ch) ch)))))

(define ^interval
  (lambda (char-from char-to)
    (const (lambda (ch)
	     (and (char<=? char-from ch)
		  (char<=? ch char-to))))))

(define <digit> (^interval #\0 #\9))
(define <uppercase-letter> (^interval #\A #\Z))
(define <lowercase-letter> (^interval #\a #\z))
(define <letter> ((disj <lowercase-letter> <uppercase-letter>)))

(define <integer>
  ((caten ((disj (char #\-) ((caten)))
	   (lambda (e) (if (null? e) "" "-")))
	  (plus <digit>))
   (lambda (sign digits)
     (string->number
      (string-append
       sign
       (list->string digits))))))

(define <sexpr>
  (lambda $
    (apply
     ((caten __
	     ((disj <boolean>
		    <char>
		    <integer>
		    <symbol>
		    <string>
		    <quote-sexpr>
		    <quasiquote-sexpr>
		    <unquote-sexpr>
		    <unquote-splicing-sexpr>
		    <vector>
		    <pair>)))
      (lambda (_ sexpr)
	sexpr))
     $)))

(define <sexpr>*
  (lambda $
    (apply
     (star ((caten __ <sexpr>)
	    (lambda (_ sexpr) sexpr)))
     $)))

(define <sexpr>+
  (lambda $
    (apply
     (plus ((caten __ <sexpr>)
	    (lambda (_ sexpr) sexpr)))
     $)))

(define <comment-to-end-of-line>
  ((caten (char #\;)
	  (star (const (lambda (ch) (not (char=? ch #\newline)))))
	  (char #\newline))))

(define <whitespace>
  (const (lambda (ch) (char<=? ch #\space))))

(define __
  (lambda $
    (apply
     (star ((disj <comment-to-end-of-line>
		  <comment-sexpr>
		  <whitespace>))
	   (lambda (_) 'skip))
     $)))

(define <comment-sexpr>
  (lambda $
    (apply
     ((caten (word "#;") __ <sexpr>))
     $)))

(define <quote-sexpr>
  ((caten __ (char #\') __ <sexpr>)
   (lambda (_1 _2 _3 sexpr)
     (list 'quote sexpr))))

(define <quasiquote-sexpr>
  ((caten __ (char #\`) __ <sexpr>)
   (lambda (_1 _2 _3 sexpr)
     (list 'quasiquote sexpr))))

(define <unquote-sexpr>
  ((caten __ (char #\,) __ <sexpr>)
   (lambda (_1 _2 _3 sexpr)
     (list 'unquote sexpr))))

(define <unquote-splicing-sexpr>
  ((caten __ (char #\,) (char #\@) __ <sexpr>)
   (lambda (_1 _2 _3 _4 sexpr)
     (list 'unquote-splicing sexpr))))

(define <vector>
  ((caten __ (char #\#) (char #\() __ <sexpr>* __ (char #\)))
   (lambda (_1 _2 _3 _4 sexprs _5 _6)
     (list->vector sexprs))))

(define <pair>
  ((disj ((caten __ (char #\() <sexpr>* __ (char #\)))
	  (lambda (_1 _2 sexprs _3 _4)
	    sexprs))
	 ((caten __ (char #\() <sexpr>+ __ (char #\.) __ <sexpr> __ (char #\)))
	  (lambda (_1 _2 sexprs _3 _4 _5 sexpr _6 _7)
	    `(,@sexprs . ,sexpr))))))

(define char-in-string
  (lambda (string . cbs)
    (let ((cb (get-callback cbs)))
      ((apply disj (map char (string->list string))) cb))))

(define <symbol>
  (plus ((disj (char-in-string "<>-+=/?!$^*_")
		<letter>
		<digit>))
	(lambda (s)
	  (string->symbol
	   (string-downcase
	    (list->string s))))))

(define <string-meta-char>
  ((disj (word "\\\\" (lambda (_) #\\))
	 (word "\\\"" (lambda (_) #\"))
	 (word "\\f" (lambda (_) #\page))
	 (word "\\n" (lambda (_) #\newline))
	 (word "\\r" (lambda (_) #\return))
	 (word "\\t" (lambda (_) #\tab))
	 ;; and just to make a point that we can define our own rich language for meta-chars:
	 (word "\\{alef}" (lambda (_) *char-alef*))
	 (word "\\{aleph}" (lambda (_) *char-alef*))
	 (word "\\{bet}" (lambda (_) *char-bet*))
	 (word "\\{gimel}" (lambda (_) *char-gimel*))
	 (word "\\{smiley}" (lambda (_) *char-smiley*))
	 (word "\\{pbuh}" (lambda (_) *char-pbuh*))
	 (word "\\{om}" (lambda (_) *char-devanagari-om*))
	 (word "\\{tibetan-om}" (lambda (_) *char-tibetan-om*))
	 ((disj
	   ((caten (word "\\x") <hex-double-byte>))
	   ((caten (word "\\x") <hex-byte>)))
	  (lambda (prefix+value)
	    (with prefix+value
	      (lambda (_prefix value)
		(integer->char value)))))
	 ((caten (word "\\o") <oct-byte>)
	  (lambda (_prefix value)
	    (integer->char value)))
	 )))

(define <string>
  ((caten (char #\")
	  (star ((disj <string-meta-char>
		       (const (lambda (ch)
				(and (char<=? #\space ch)
				     (not (char=? ch #\")))))))
		(lambda (s)
		  (list->string s)))
	  (char #\"))
   (lambda (_1 string _2) string)))
