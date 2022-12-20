;;;; laboratorio7.lisp
;;;; Ficha de Laboratório nº7 - Apoio ao 1º projeto
;;;; Autor: 


;;; Tabuleiro

(defun tabuleiro-teste ()
  "Retorna um tabuleiro 3x3 (3 arcos na vertical por 3 arcos na horizontal)"
	'(
		((0 0 0) (0 0 1) (0 1 1) (0 0 1))
		((0 0 0) (0 1 1) (1 0 1) (0 1 1))
	)
)

;;; Exercicios
;; get-arcos-horizontais
;; (get-arcos-horizontais (tabuleiro-teste))
(defun get-arcos-horizontais (tabuleiro)
  (first tabuleiro)
)

;; get-arcos-verticais
;; (get-arcos-verticais (tabuleiro-teste))
(defun get-arcos-verticais (tabuleiro)
  (second tabuleiro)
)

;; get-arco-na-posicao
;; (get-arco-na-posicao 2 3 (get-arcos-horizontais (tabuleiro-teste)))
(defun get-arco-na-posicao (n m arcos)
  (nth (1- m) (nth (1- n) arcos))
)

;; substituir
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

;; arco-na-posicao
;; (arco-na-posicao 2 2 (get-arcos-horizontais (tabuleiro-teste)))
;; (arco-na-posicao 4 1 (get-arcos-verticais (tabuleiro-teste)))
(defun arco-na-posicao (n m arcos &optional (valor 1))
  (cond
   ((null arcos) nil)
   ((= n 1) (cons (substituir m (car arcos) valor) (cdr arcos)))
   (t (cons (car arcos) (arco-na-posicao (1- n) m (cdr arcos) valor)))
  )
)

;; arco-horizontal 
;; (arco-horizontal 3 1 (tabuleiro-teste))
;; (arco-horizontal 3 2 (tabuleiro-teste))
;; (arco-horizontal 7 2 (tabuleiro-teste))
(defun arco-horizontal (n m tabuleiro)
  (cond
   ((null tabuleiro) nil)
   (t (let*
          ((arcos (get-arcos-horizontais tabuleiro)))
        (cond
         ((null arcos) nil)
         ((> n (list-length arcos)) nil)
         ((> m (list-length (nth (1- n) arcos))) nil)
         ((= (get-arco-na-posicao n m (get-arcos-horizontais tabuleiro)) 1) nil)
         (t (list (arco-na-posicao n m arcos) (get-arcos-verticais tabuleiro)))
        )
      )
    )
  )
)

;; arco-vertical
;; (arco-vertical 1 2 (tabuleiro-teste))
;; (arco-vertical 2 2 (tabuleiro-teste))
;; (arco-vertical 5 5 (tabuleiro-teste))
(defun arco-vertical (n m tabuleiro)
  (cond
   ((null tabuleiro) nil)
   (t (let*
          ((arcos (get-arcos-verticais tabuleiro)))
        (cond
         ((null arcos) nil)
         ((> n (list-length arcos)) nil)
         ((> m (list-length (nth (1- n) arcos))) nil)
         ((= (get-arco-na-posicao n m (get-arcos-verticais tabuleiro)) 1) nil)
         (t (list (get-arcos-horizontais tabuleiro) (arco-na-posicao n m arcos)))
        )
      )
    )
  )
)
