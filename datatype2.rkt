; define constructor ,test-varient ,extract data-function from undelying data from constuctor

; defineing constructor

(define (Const i) (list 'Const i))
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2))
(define (Multiply e1 e2) (list 'Multiply e1 e2))

;;test-varient function to check which type constrct data made from ;

(define (Const? x) (eq? (car x) 'Const))
(define (Negate? x) (eq? (car x) 'Negate))
(define (Add? x) (eq? (car x) 'Add))
(define (Multiply? x) (eq? (car x) 'Multiply))

;; get data from underlying constructor

(define (Const-int e) (car (cdr e)))
(define (Negate-e e) (car (cdr e)))
(define (Add-e1 e) (car (cdr e)))
(define (Add-e2 e) (car (cdr (cdr e))))
(define (Multiply-e1 e) (car (cdr e)))
(define (Multiply-e2 e) (car (cdr (cdr e))))


(define (eval-exp e) (cond [(Const? e) e]
                           [(Negate? e) (Const (- (Const-int (eval-exp (Negate-e e)))))]
                           [(Add? e) (let ([v1 (Const-int (eval-exp(Add-e1 e)))][v2 (Const-int (eval-exp (Add-e2 e)))]) (Const (+ v1 v2)))]
                           [(Multiply? e) (let ([v1 (Const-int (eval-exp(Multiply-e1 e)))] [v2 (Const-int (eval-exp (Multiply-e2 e)))]) (Const (* v1 v2)))]
                           [else (error "eval-exp expected an exp")]))


(eval-exp (Multiply (Const 3) (Const 4)))


;another method using struct datatype

(struct Const (int) #:transparent)
(struct Add (e1 e2) #:transparent)
(struct Negate ( e) #:transparent)
(struct Multiply (e1 e2) #:transparent)

;struct provide a function Const? or made by this construct (test-varient)
; we can access data (Const-int expression) from data

(Const 3)


(define (eval-exp e) (cond [(Const? e) e]
                           [(Negate? e) (Const (- (Const-int (eval-exp (Negate-e e)))))]
                           [(Add? e) (let ([v1 (Const-int (eval-exp(Add-e1 e)))][v2 (Const-int (eval-exp (Add-e2 e)))]) (Const (+ v1 v2)))]
                           [(Multiply? e) (let ([v1 (Const-int (eval-exp(Multiply-e1 e)))] [v2 (Const-int (eval-exp (Multiply-e2 e)))]) (Const (* v1 v2)))]
                           [else (error "eval-exp expected an exp")]))
