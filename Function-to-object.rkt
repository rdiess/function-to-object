#lang racket

;Ray Diess

;This is as far as I'm going to get. Let, Call, and Lamda do not work and variables are not defined correctly, among other problems.

;The idea for implementing let was to have binary op and unary op
;expressions return a list of strings, which would then be returned by lamda, bound to a variable by let and stored
;in the environment, then "unpacked" by call, replaced by the id expression in call and then run with the exp expression in call.
;I was going to try to get this method to work for NOT first, that's why the match expression looks so strange for NOT, LAMDA
;and CALL.
;this would've been inefficient if it had worked, so I look forward to finding out the better way to do this.

;anyway...



(require parser-tools/lex
         parser-tools/yacc)

;Tokens
(define-tokens v (ID
                  VALUE
                  VAR))
(define-empty-tokens e (LEFTPAREN
                        RIGHTPAREN
                        AND
                        OR
                        XOR
                        UNARYOP
                       ; VALUE
                        EXPR
                        WITH
                        THEN
                        IF
                        LET
                        ELSE
                        IN
                        CALL
                        LAMDA
                        EOF))
;lexer
(define thelexer
  (lexer
    
   ["NOT" (token-UNARYOP)]
   ["OR" (token-OR)]
   ["AND" (token-AND)]
   ["XOR" (token-XOR)]
   [#\( (token-LEFTPAREN)]
   [#\) (token-RIGHTPAREN)]
   ["TRUE" (token-VALUE #t)]
   ["FALSE" (token-VALUE #f)]
   [#\, (token-EXPR)]
   ["LET" (token-LET)]
   ["IN" (token-IN)]
   ["IF" (token-IF)]
   ["THEN" (token-THEN)]
   ["ELSE" (token-ELSE)]
   ["CALL" (token-CALL)]
   ["WITH" (token-WITH)]
   ["LAMDA" (token-LAMDA)]
   [numeric (token-ID lexeme)]
   [alphabetic (token-VAR lexeme) ]
   [(repetition 1 +inf.0 (char-range #\a #\z)) (list 'token-VAR lexeme)] ;ask jeff if all caps is okay to avoid
   [whitespace (thelexer input-port)]
   [(eof) (token-EOF)]
    ))

;structs
(struct andExpr (argOne argTwo) #:transparent)
(struct orExpr (argOne argTwo) #:transparent)
(struct xorExpr (argOne argTwo) #:transparent)
(struct lamExpr (argOne argTwo) #:transparent)
(struct letExpr (argOne argTwo argThree) #:transparent)
(struct letwlamExpr (argOne argTwo argThree) #:transparent)
(struct ifExpr (argOne argTwo argThree) #:transparent)
(struct callExpr (argOne argTwo) #:transparent)
(struct unExpr (argOne) #:transparent)
(struct idExpr (id) #:transparent)
(struct valExpr (val) #:transparent)
(struct varExpr (var) #:transparent)
(struct closureExpr (vars lamExpr env) #:transparent)

;parser
(define expparser
  (parser
    (start exp)
    (end EOF)
    (tokens v e)
    (error void)
    (grammar 
       (exp
          ((LEFTPAREN exp AND exp RIGHTPAREN) (andExpr $2 $4)) ;Parser +1, Eval +1
          ((LEFTPAREN exp OR exp RIGHTPAREN) (orExpr $2 $4))   ;Parser +1, Eval +1
          ((LEFTPAREN exp XOR exp RIGHTPAREN) (xorExpr $2 $4)) ;Parser +1, Eval +1
          ((LEFTPAREN UNARYOP exp RIGHTPAREN) (unExpr $3))     ;Parser +1, Eval +1
          ((LEFTPAREN LET LEFTPAREN exp exp RIGHTPAREN IN exp RIGHTPAREN) (letExpr $4 $5 $8)) ;parser +1, Eval +1
          ((LEFTPAREN IF exp  THEN  exp  ELSE  exp RIGHTPAREN) (ifExpr $3 $5 $7)) ;Parser +1, 
          ((LEFTPAREN CALL exp WITH exp RIGHTPAREN) (callExpr $3 $5)) ;parser +1 
          ((LEFTPAREN LAMDA LEFTPAREN exp RIGHTPAREN exp RIGHTPAREN) (lamExpr $4 $6)) ;parser +1
          
          ((LEFTPAREN LET LEFTPAREN exp
                      LEFTPAREN LEFTPAREN LAMDA LEFTPAREN exp RIGHTPAREN exp RIGHTPAREN exp RIGHTPAREN
                      RIGHTPAREN IN
                      CALL exp WITH exp RIGHTPAREN
                      RIGHTPAREN) (letwlamExpr $4 $9 $11 $13 $18 $20))
                      
                      
          ((ID) (idExpr (string->number $1)))
          ((VALUE) (valExpr $1))
          ((VAR) (varExpr $1))
          
          )
       
       )
    ))

(define (lex-this lexer input)
  (lambda () (lexer input)))


;the evaluate function
(define (evaluate aTree)
  ;(eval aTree (empty-env))
  ;the eval function for functions where an enviroment is relevant
  (define (eval env aTree )
(match aTree
  
    [(letExpr a b c) (extend-env (quote a) b (empty-env))
                        (evaluate c)]

 
    

     
     [(callExpr a b) (cond
                       [(equal? b #t) (evaluate(string-join(append (first(apply-env (empty-env) a)) "TRUE")))]
                       [else ((evaluate(string-join(append (first(apply-env (empty-env) a)) "FALSE"))))]
                       )]
    

    )
    
    )
  (match aTree 
    
        [(andExpr a b) (and (evaluate a) 
                             (evaluate b))]

        [(orExpr a b) (or (evaluate a) 
                             (evaluate b))]
    
        [(xorExpr a b) (not
                        (equal?
                         (evaluate a)
                         (evaluate b))
                        )] 
    
        [(unExpr a)  (cond
                       [(equal? a (varExpr "a")) (cons "NOT" (list (evaluate a)))] ; just returns string a
                      [else (not (evaluate a))]
                      )]
                         
                            

        [(ifExpr a b c) (cond
                          [(equal? (evaluate a) #t) (evaluate b)]
                          [(equal? (evaluate a) #\T) (evaluate b)]
                           [else (evaluate c)])] 

       [(lamExpr a b) (evaluate b)
                      
                        ]

    [(letExpr a b c) (extend-env (quote a) b (empty-env))
                        (evaluate c)]


       

       [(callExpr a b) (cond
                       [(equal? b #t) (evaluate(string-join(append (first(apply-env (empty-env) a)) "TRUE")))]
                       [else ((evaluate(string-join(append (first(apply-env (empty-env) a)) "FALSE"))))]
                       )]
    

      
                        
                                     
        [(idExpr a) a]
     [(valExpr a) a]
        [(varExpr a) a]
       ))

; Implementation of Environment

(define (empty-env)
  (lambda (searchVar)
    (error "No Binding Found" searchVar)))

(define (extend-env savedVar savedVal savedEnv)
  (lambda (searchVar)
    (if (equal? searchVar savedVar)
        savedVal
        (apply-env savedEnv searchVar))))

(define (apply-env env searchVar)
  (env searchVar))





;helper function for an input string
(define (evaluateString someString)
  (define input (open-input-string someString))
  (evaluate (expparser (lex-this thelexer input))))

;helper function for an input file
(define (evaluateFile someFile)
  (define input (open-input-file someFile))
  (evaluate (expparser (lex-this thelexer input))))
                        
 ;some tests I ran 
(define input (open-input-string "(TRUE AND (NOT FALSE))"))
(define andinput (open-input-string "(TRUE AND (NOT TRUE))"))
(define input2  (open-input-string "(NOT aaa)"))
(define xorinput  (open-input-string "(TRUE XOR TRUE)"))
(define orinput  (open-input-string "(TRUE OR TRUE)"))
(define ifinput  (open-input-string "(IF (TRUE OR TRUE) THEN(TRUE AND TRUE) ELSE(TRUE XOR TRUE))"))

(define letinput (open-input-string "(LET (a (LAMDA (x) (NOT x))) IN (CALL a WITH TRUE))" empty-env)) ;works
(define callinput (open-input-string "(CALL a WITH (TRUE XOR TRUE))")) ;works
(define laminput  (open-input-string "(LAMDA (a) (NOT a))")) ;works

(define letnovarinput (open-input-string "(LET (a (TRUE AND FALSE)) IN ((NOT a) AND (NOT a)))"))

;test evaluate: (evaluate (expparser (lex-this thelexer input))) 




;test lexer: (getTokens thelexer input)
;testParser: (expparser (lex-this thelexer input))



;;;;;;;;;;;A bunch of notes I wrote while working.


;parenthesize everything, and precedence doesn't matter
;let extends env

;(let(foo(lamda (x) (foo)) in (call foo with false)) binds value to foo

;letExpr name(foo) value(lamda) body

;for and: evaluate first part, then second part, and then do a racket and

;apply racket functions in evaluate (so for not, apply racket's not))

;when you see a let, bind the value to the variable (using env)

;(define (eval program)
;(evaluate program (empt-env)
;(evaluate program env
;(match
;(andExp ...)
;(and (evaluate op1 env)
;(evaluate op2 env)


;eventually your going to get down to something that's just "true" or "false"
; the base cases could be true, false or x

;valueExpr is just true or false

;so for and, just make sure the operands are evaluated in the right way

;then do let to do binding.

;you start with an empty env and extend it as you go along

;int x = 5, you're adding this declaration to the env you have.

;every time you see a let, you should extend your env

; (extend-env(foo true) (empty-env)) ->bind foo to true and add this to the empty-env (this is like "ring 1" of the env)

;this process of creating and adding to an environment is basically implementing a stack.

;do an eval in the old environment, then bind it, and pass the next one and then keep going.
;you just have to figure out how to do one step, the rest is recursive and will take care of itself

;test everything without variables first, then simple variables, then do function stuff.