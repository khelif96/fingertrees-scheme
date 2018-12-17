(define (atom? a)
  ( and (not (pair? a)) (not (null? a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIGIT STRUCTURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (digit i)
  (if (list? i) i (list i)))

(define (digit-append i digit)
  (cond ((list? i)(append digit i))
        (else (append digit (list i)))
        )
  )

;; RUNTIME OF THIS IS still O(1) still because length(digit) <= 4 always.
(define (count-digits digit)
  (define (iter count digit)
  (if (null? digit) count
      (iter (+ 1 count) (cdr digit))
  ))
  (iter 0 digit)
  )

;; Pre condition: Digit up to length N where 0 < N <= 4.  Digit cant be empty
;; Post condition: Digits up to N-1 is outputed
(define (digit-head digit)

  (define (iter count acc digit)
  (if (= count 3) acc
      (iter (+ 1 count) (append acc (list (car digit))) (cdr digit))
  ))
  (iter 0 '() digit)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NODE STRUCTURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NODE2 and NODE3
;; Dont need this for now. Maybe later
;; TODO: Optimize this.  Don't need to append always. Can directly be list
(define (node2 item1 item2)
  (digit-append item2 (digit item1))
  )

(define (node3 item1 item2 item3)
  (digit-append (list item2 item3) (digit item1))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FINGER TREE STRUCTURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-finger-tree item)
  (cond ((null? item) empty-tree)
        ( (null? (cdr item)) (initilaize-single item))
  )
)


(define (finger-tree-push item finger-tree)
  ( cond ((isEmpty? finger-tree) (push-empty item))
          ((isSingle? item) (push-single item finger-tree))
          (else
           (deep-append item finger-tree)))
  )

(define (finger-tree-left finger-tree)
  (cond ( (atom? finger-tree) finger-tree)
        ( (null? finger-tree) finger-tree)
        ( (list? (cadr finger-tree)) (car finger-tree))
        ( else finger-tree))
  )

(define (finger-tree-right finger-tree)
  (cond
    ( (null? finger-tree) finger-tree)
    ( (list? (cadr finger-tree)) (caddr finger-tree))
    ( else empty-tree)
      )

  )

(define (finger-tree-spine finger-tree)
  (cond ( (atom? finger-tree) finger-tree)
        ( (null? finger-tree) finger-tree)
        ( (list? (cadr finger-tree)) (cadr finger-tree))
        ( else empty-tree))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  EMPTY TREE STUCTURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define empty-tree '())

(define (push-empty item)
  (initilaize-single item)
  )
(define (isEmpty? fingerTree)
  ( null? fingerTree)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SINGLE  TREE STRUCTURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (initilaize-single item)
  (digit item)
  )

(define (push-single item single-tree)
  (deep (finger-tree-left single-tree)
        (finger-tree-push (finger-tree-right single-tree) (finger-tree-spine single-tree))
        (digit item))
  )
(define (isSingle? finger-tree)
  (not (list? (cadr finger-tree)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DEEP TREE STRUCTURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deep left spine right )
  (list left spine right)
  )


(define (deep-append item finger-tree)
  (cond
    ((< (length (finger-tree-right finger-tree)) 4)
     (if (list? item)
         (deep (finger-tree-left finger-tree)(finger-tree-push (finger-tree-right finger-tree) (finger-tree-spine finger-tree)) item)
         (deep (finger-tree-left finger-tree) (finger-tree-spine finger-tree) (digit-append item (finger-tree-right finger-tree))))

     ) 
  (else
   (deep (finger-tree-left finger-tree)
        (finger-tree-push (digit-head (finger-tree-right finger-tree)) (finger-tree-spine finger-tree))
         (digit-append item (digit(list-ref (finger-tree-right finger-tree) 3))))
  )

  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;pre : takes a finger tree and removes the items seeing hoe the finger tree is
;post : returns a tree after poping the element
(define (finger-tree-pop finger-tree)
  (cond
    ((null? finger-tree) '())
    ((or (atom?(car finger-tree))(and (null? (finger-tree-right finger-tree))(null? (finger-tree-spine finger-tree)))) (cons (car finger-tree)(list (cdr finger-tree))))
  (else
   (let ((first-value (car (finger-tree-left finger-tree)))(left-rem-value (cdr (finger-tree-left finger-tree))))
     (cond
       ((and (> (length left-rem-value) 0))
          (cons first-value (list (deep left-rem-value (finger-tree-spine finger-tree) (finger-tree-right finger-tree)))))
       ((= (length left-rem-value) 0)
            (cond
              ((null? (finger-tree-spine finger-tree))
                (if (= (length (finger-tree-right finger-tree)) 1 )
                    (cons first-value (list (finger-tree-right finger-tree)))
                    (cons first-value (list (deep (list (car (finger-tree-right finger-tree))) (finger-tree-spine finger-tree) (cdr (finger-tree-right finger-tree)))))))
               (else(cons first-value  (list (deep-pop finger-tree))))))
       (else (cons (car finger-tree)(list (cdr finger-tree))))))))) 

;pre: inputs a finger tree with one element in the left finger and non empty spine and non empty right finger
;post: outputs the tree and pop'ed element after poping the element
(define(deep-pop finger-tree)
  (list (cons (car (finger-tree-pop (finger-tree-spine finger-tree))) '())(cadr (finger-tree-pop (finger-tree-spine finger-tree)))(finger-tree-right finger-tree)))
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define finger-tree empty-tree)
finger-tree
(define finger-tree (push-empty '(20 18 17 16)))
finger-tree
(define finger-tree (push-single '(30 29 28 27) finger-tree))
finger-tree
(define finger-tree (deep-append '40 finger-tree))
finger-tree
(define finger-tree (deep-append '50 finger-tree))
finger-tree
(define finger-tree (deep-append '60 finger-tree))
finger-tree
(define finger-tree (deep-append '70 finger-tree))
finger-tree
(define finger-tree (deep-append '80 finger-tree))
finger-tree
(define finger-tree (deep-append '90 finger-tree))
finger-tree
(define finger-tree (deep-append '100 finger-tree))
finger-tree
(define finger-tree (deep-append '101 finger-tree))
finger-tree
(define finger-tree (deep-append '102 finger-tree))
finger-tree
(define finger-tree (deep-append '103 finger-tree))
finger-tree
(define finger-tree (deep-append '104 finger-tree))
finger-tree
(define finger-tree (deep-append '105 finger-tree))
finger-tree
(define finger-tree (deep-append '106 finger-tree))
finger-tree

; Test Popping
(finger-tree-pop finger-tree) ; Demoing sample output of pop function returning popped value and resulting tree
(car (finger-tree-pop finger-tree)) ; Demoing just getting the value popped
(define finger-tree (cdr (finger-tree-pop finger-tree))) ; Saving resulting finger tree after pop
finger-tree
;

(finger-tree-pop '((18) ((30) ((27) () (60 70 80)) (90 102 103 104)) (105 106)))
;this has two spines and in all its spine have length one along with the first left finger, following was the output
;output:
;(18 ((30) ((27) ((60) () (70 80)) (90 102 103 104)) (105 106)))
;18 is the pop'ed element
;((30) ((27) ((60) () (70 80)) (90 102 103 104)) (105 106)) is the remaining tree

;Output:
;()
;(20 18 17 16)
;((20 18 17 16) () (30 29 28 27))
;((20 18 17 16) (30 29 28) (27 40))
;((20 18 17 16) (30 29 28) (27 40 50))
;((20 18 17 16) (30 29 28) (27 40 50 60))
;((20 18 17 16) ((30 29 28) () (27 40 50)) (60 70))
;((20 18 17 16) ((30 29 28) () (27 40 50)) (60 70 80))
;((20 18 17 16) ((30 29 28) () (27 40 50)) (60 70 80 90))
;((20 18 17 16) ((30 29 28) (27 40 50) (60 70 80)) (90 100))
;((20 18 17 16) ((30 29 28) (27 40 50) (60 70 80)) (90 100 101))
;((20 18 17 16) ((30 29 28) (27 40 50) (60 70 80)) (90 100 101 102))
;((20 18 17 16) ((30 29 28) ((27 40 50) () (60 70 80)) (90 100 101)) (102 103))
;((20 18 17 16) ((30 29 28) ((27 40 50) () (60 70 80)) (90 100 101)) (102 103 104))
;((20 18 17 16) ((30 29 28) ((27 40 50) () (60 70 80)) (90 100 101)) (102 103 104 105))
;((20 18 17 16) ((30 29 28) ((27 40 50) (60 70 80) (90 100 101)) (102 103 104)) (105 106))
;(20 ((18 17 16) ((30 29 28) ((27 40 50) (60 70 80) (90 100 101)) (102 103 104)) (105 106)))
;20
;(((18 17 16) ((30 29 28) ((27 40 50) (60 70 80) (90 100 101)) (102 103 104)) (105 106)))
;(18 ((30) ((27) ((60) () (70 80)) (90 102 103 104)) (105 106)))