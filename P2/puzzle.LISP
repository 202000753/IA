;;;; puzzle.lisp
;;;; Opera��es do problema
;;;; Autores: Nuno Reis, Vitor Nunes

;; (tabuleiro-de-jogo)
(defun tabuleiro-de-jogo ()
  '( 
    (
     (1 2 1 1 0 2) 
     (2 1 1 1 1 0) 
     (0 2 1 1 2 0) 
     (0 1 0 2 2 0) 
     (1 2 0 0 0 0) 
     (0 1 2 1 2 1)
     ) 
    (
     (1 0 1 0 0) 
     (2 1 1 2 2) 
     (2 1 1 2 0) 
     (1 2 2 1 1) 
     (1 2 2 0 0) 
     (0 1 2 1 2) 
     (2 2 1 2 0) 
     )
   )
)

;; (get-arcos-horizontais (tabuleiro-teste))
(defun get-arcos-horizontais (estado)
  (first estado)
)

;; (get-arcos-verticais (tabuleiro-teste))
(defun get-arcos-verticais (estado)
  (second estado)
)

;; (get-arco-na-posicao 2 3 (get-arcos-horizontais (tabuleiro-teste)))
(defun get-arco-na-posicao (n m arcos)
  (cond
   ((or (< (1- m) 0) (< (1- n) 0)) 0)
   (t (nth (1- m) (nth (1- n) arcos)))
  )
)

;; (substituir 1 (car (get-arcos-horizontais (tabuleiro-teste))))
;; (substituir 2 (car (get-arcos-verticais (tabuleiro-teste))) 2)
(defun substituir (n arcos &optional (valor 1))
  (cond
   ((null arcos) nil)
   ((> n (list-length arcos)) nil)
   ((= n 1) (cons valor (cdr arcos)))
   (t (cons (car arcos) (substituir (1- n) (cdr arcos) valor)))
  )
)

;; (arco-na-posicao 2 2 (get-arcos-horizontais (tabuleiro-teste)))
;; (arco-na-posicao 4 1 (get-arcos-verticais (tabuleiro-teste)))
(defun arco-na-posicao (n m arcos &optional (valor 1))
  (cond
   ((null arcos) nil)
   ((= n 1) (cons (substituir m (car arcos) valor) (cdr arcos)))
   (t (cons (car arcos) (arco-na-posicao (1- n) m (cdr arcos) valor)))
  )
)

;; (arco-horizontal 3 1 (tabuleiro-teste))
;; (arco-horizontal 3 2 (tabuleiro-teste))
;; (arco-horizontal 7 2 (tabuleiro-teste))
(defun arco-horizontal (n m tabuleiro valor)
  (cond
   ((null tabuleiro) nil)
   (t (let*
          ((arcos (get-arcos-horizontais tabuleiro)))
        (cond
         ((null arcos) nil)
         ((> n (list-length arcos)) nil)
         ((> m (list-length (nth (1- n) arcos))) nil)
         ((/= (get-arco-na-posicao n m (get-arcos-horizontais tabuleiro)) 0) nil)
         (t (list (arco-na-posicao n m arcos valor) (get-arcos-verticais tabuleiro)))
        )
      )
    )
  )
)

;; (arco-vertical 1 2 (tabuleiro-teste))
;; (arco-vertical 2 2 (tabuleiro-teste))
;; (arco-vertical 5 5 (tabuleiro-teste))
(defun arco-vertical (n m tabuleiro valor)
  (cond
   ((null tabuleiro) nil)
   (t (let*
          ((arcos (get-arcos-verticais tabuleiro)))
        (cond
         ((null arcos) nil)
         ((> n (list-length arcos)) nil)
         ((> m (list-length (nth (1- n) arcos))) nil)
         ((/= (get-arco-na-posicao n m (get-arcos-verticais tabuleiro)) 0) nil)
         (t (list (get-arcos-horizontais tabuleiro) (arco-na-posicao n m arcos valor)))
        )
      )
    )
  )
)

;(n-caixas-fechadas (get-arcos-horizontais (problema-b)) (get-arcos-verticais (problema-b)) (1- (list-length (get-arcos-horizontais (problema-b)))) (1- (list-length (get-arcos-verticais (problema-b)))))
(defun n-caixas-fechadas (horizontais verticais n-horizontais n-verticais &optional (nh 1) (nv 1) (n 0))
  (cond
   ((and (= nh n-horizontais) (= nv n-verticais)) (cond  
                                                   ((and (/= (get-arco-na-posicao nh nv horizontais) 0) (/= (get-arco-na-posicao (1+ nh) nv horizontais) 0) (/= (get-arco-na-posicao nv nh verticais) 0) (/= (get-arco-na-posicao (1+ nv) nh verticais) 0)) 1)
                                                   (t 0)
                                                   ))
   (t
    (+ n 
       (cond
        ((and (/= (get-arco-na-posicao nh nv horizontais) 0) (/= (get-arco-na-posicao (1+ nh) nv horizontais) 0) (/= (get-arco-na-posicao nv nh verticais) 0) (/= (get-arco-na-posicao (1+ nv) nh verticais) 0)) 1)
        (t 0)
       )
       (cond
        ((/= nv n-verticais) (n-caixas-fechadas horizontais verticais n-horizontais n-verticais nh (1+ nv) n))
        (t (n-caixas-fechadas horizontais verticais n-horizontais n-verticais (1+ nh) 1 n))
       )
    )
   )
  )
)

;(n-cantos (get-arcos-horizontais (problema-b)) (get-arcos-verticais (problema-b)) (list-length (get-arcos-horizontais (problema-b))) (1- (list-length (get-arcos-verticais (problema-b)))))
(defun n-cantos (horizontais verticais n-horizontais n-verticais &optional (nh 1) (nv 1) (n 0))
  (cond
   ((and (= nh n-horizontais) (= nv n-verticais)) (+ 
                                                   (cond  
                                                    ((and (eq (get-arco-na-posicao nh nv horizontais) 1) (eq (get-arco-na-posicao nv (1- nh) verticais) 1)) 1)
                                                    (t 0)
                                                   )
                                                   (cond  
                                                    ((and (eq (get-arco-na-posicao nh nv horizontais) 1) (eq (get-arco-na-posicao (1+ nv) (1- nh) verticais) 1)) 1)
                                                    (t 0)
                                                   )
                                                   (cond  
                                                    ((and (eq (get-arco-na-posicao nh nv horizontais) 1) (eq (get-arco-na-posicao nv nh verticais) 1)) 1)
                                                    (t 0)
                                                   )
                                                   (cond  
                                                    ((and (eq (get-arco-na-posicao nh nv horizontais) 1) (eq (get-arco-na-posicao (1+ nv) nh verticais) 1)) 1)
                                                    (t 0)
                                                   )
                                                  ))
   (t
    (+ n 
       (cond  
        ((and (eq (get-arco-na-posicao nh nv horizontais) 1) (eq (get-arco-na-posicao nv (1- nh) verticais) 1)) 1)
        (t 0)
       )
       (cond  
        ((and (eq (get-arco-na-posicao nh nv horizontais) 1) (eq (get-arco-na-posicao (1+ nv) (1- nh) verticais) 1)) 1)
        (t 0)
       )
       (cond  
        ((and (eq (get-arco-na-posicao nh nv horizontais) 1) (eq (get-arco-na-posicao nv nh verticais) 1)) 1)
        (t 0)
       )
       (cond  
        ((and (eq (get-arco-na-posicao nh nv horizontais) 1) (eq (get-arco-na-posicao (1+ nv) nh verticais) 1)) 1)
        (t 0)
       )
       (cond
        ((/= nv n-verticais) (n-cantos horizontais verticais n-horizontais n-verticais nh (1+ nv) n))
        (t (n-cantos horizontais verticais n-horizontais n-verticais (1+ nh) 1 n))
       )
    )
   )
  )
)

(defun tabuleiro-preenchidop (tabuleiro)
  (cond
   ((or (equal (zeros-na-lista (first tabuleiro)) t) (equal (zeros-na-lista (second tabuleiro)) t)) nil)
   (t t)
  )
)

(defun zeros-na-lista (lista)
  (cond
   ((equal lista nil) nil)
   ((equal (listp lista) t) (cond
                             ((or (equal (zeros-na-lista (car lista)) t) (equal (zeros-na-lista (cdr lista)) t)) t)
                             (t nil)
                            ))
   ((= lista 0) t)
   (t nil)
  )
)