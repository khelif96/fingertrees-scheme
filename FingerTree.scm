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
          ((isSingle? finger-tree) (push-single item finger-tree))
          (else
           (deep-append item finger-tree)))
  )

(define (finger-tree-left finger-tree)
  (cond ( (atom? finger-tree) finger-tree)
        ( (null? finger-tree) finger-tree)
        ( (isSingle? finger-tree) finger-tree)
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
  (let ((len (length single-tree)))
          (if (and ( < len 4) (< (+ len (length (digit item))) 5))
              (append  single-tree (digit item))
              (deep (finger-tree-left single-tree)
                    (finger-tree-push (finger-tree-right single-tree) (finger-tree-spine single-tree))
                    (digit item))
              )
        )

  )
(define (isSingle? finger-tree)
  (let ((len (length finger-tree)))
    (if (< len 3)
        #t
         (not (list? (cadr finger-tree)))
        )

    )
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

;deep pop stores the value of the finger-tree-pop using let and gets the first element by car which is the removed element
;the second element is the cadr of the result which is the remaining of the list
(define(deep-pop finger-tree)
  (list (cons (car (finger-tree-pop (finger-tree-spine finger-tree))) '())(cadr (finger-tree-pop (finger-tree-spine finger-tree)))(finger-tree-right finger-tree)))
  

(define (fill-up count finger-tree)
  (if (= count 0) finger-tree
      (fill-up (- count 1) (finger-tree-push count finger-tree))
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
(define finger-tree empty-tree)
finger-tree
(define finger-tree (finger-tree-push '40 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '41 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '42 finger-tree))
(define finger-tree (finger-tree-push '43 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '44 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '45 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '46 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '47 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '48 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '49 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '50 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '51 finger-tree))

;(define finger-tree (fill-up 2000 finger-tree))
;(caddr finger-tree)
(define finger-tree (finger-tree-push '52 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '53 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '54 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '55 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '56 finger-tree))
finger-tree
(define finger-tree (finger-tree-push '57 finger-tree))
finger-tree


(define finger-tree (finger-tree-pop finger-tree))
(cadr finger-tree)
(define finger-tree (finger-tree-pop (cadr finger-tree)))
(cadr finger-tree)
(define finger-tree (finger-tree-pop (cadr finger-tree)))
(cadr finger-tree)
(define finger-tree (finger-tree-pop (cadr finger-tree)))
(cadr finger-tree)
(define finger-tree (finger-tree-pop (cadr finger-tree)))
(cadr finger-tree)
(define finger-tree (finger-tree-pop (cadr finger-tree)))
(cadr finger-tree)
(define finger-tree (finger-tree-pop (cadr finger-tree)))
(cadr finger-tree)
(define finger-tree (finger-tree-pop (cadr finger-tree)))
(cadr finger-tree)
(define finger-tree (finger-tree-pop (cadr finger-tree)))
(cadr finger-tree)
(define finger-tree (finger-tree-pop (cadr finger-tree)))
(cadr finger-tree)
;
;; Test Popping
;(finger-tree-pop finger-tree) ; Demoing sample output of pop function returning popped value and resulting tree
;(car (finger-tree-pop finger-tree)) ; Demoing just getting the value popped
;(define finger-tree (cadr (finger-tree-pop finger-tree))) ; Saving resulting finger tree after pop
;finger-tree
;;
;
;(finger-tree-pop '((18) ((30) ((27) () (60 70 80)) (90 102 103 104)) (105 106)))
;this has two spines and in all its spine have length one along with the first left finger, following was the output
;output:
;(18 ((30) ((27) ((60) () (70 80)) (90 102 103 104)) (105 106)))
;18 is the pop'ed element
;((30) ((27) ((60) () (70 80)) (90 102 103 104)) (105 106)) is the remaining tree

;Output:
;((40 41 42 43) (44 45 46) (47 48))
;((40 41 42 43) (44 45 46) (47 48 49))
;((40 41 42 43) (44 45 46) (47 48 49 50))
;((40 41 42 43) ((44 45 46) () (47 48 49)) (50 51 52))
;((40 41 42 43) ((44 45 46) () (47 48 49)) (50 51 52 53))
;((40 41 42 43) ((44 45 46) (47 48 49) (50 51 52)) (53 54))
;((40 41 42 43) ((44 45 46) (47 48 49) (50 51 52)) (53 54 55))
;((40 41 42 43) ((44 45 46) (47 48 49) (50 51 52)) (53 54 55 56))
;((40 41 42 43) ((44 45 46) ((47 48 49) () (50 51 52)) (53 54 55)) (56 57))
;((41 42 43) ((44 45 46) ((47 48 49) () (50 51 52)) (53 54 55)) (56 57))
;((42 43) ((44 45 46) ((47 48 49) () (50 51 52)) (53 54 55)) (56 57))
;((43) ((44 45 46) ((47 48 49) () (50 51 52)) (53 54 55)) (56 57))
;((44) ((45 46) ((47 48 49) () (50 51 52)) (53 54 55)) (56 57))
;((45) ((46) ((47 48 49) () (50 51 52)) (53 54 55)) (56 57))
;((46) ((47) ((48 49) () (50 51 52)) (53 54 55)) (56 57))
;((47) ((48) ((49) () (50 51 52)) (53 54 55)) (56 57))
;((48) ((49) ((50) () (51 52)) (53 54 55)) (56 57))
;((49) ((50) ((51) () (52)) (53 54 55)) (56 57))
;((50) ((51) (52) (53 54 55)) (56 57))
