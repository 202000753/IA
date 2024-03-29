;;;; laboratorio5.lisp
;;;; Ficha laboratorial sobre funcoes de alto nivel em Lisp
;;;; Autor: Nuno Reis


;;; Exercicio Introdutorio  - funcall + lambda
;;(remover-se #'(lambda (x) (= x 0)) '(1 2 0 2 0 4))
(defun remover-se(pred lista)
  (cond 
   ((null lista) NIL) 
   ((funcall pred (car lista)) (remover-se pred (cdr lista)))
   (T (cons (car lista) (remover-se pred (cdr lista))))
  )
)

;;; Exercicios - Parametros de lambda
;;(media 1 2 34 )
;; 12.333333
(defun media (&rest lista)
  (cond
   ((null lista) nil)
   (t (/ (apply #'+ lista) (length lista)))
  )
)

;; (coluna ((1 2 3) (4 5 6) (7 8 9)))
;; (1 4 7)
(defun coluna (m1)
  (mapcar #'(lambda (linha &aux (cabeca (car linha))) cabeca) m1)
)

;; (aplicar-simbolo 'mod  2 3)
;; 2
;; (aplicar-simbolo '*  2 3)
;; 6
(defun aplicar-simbolo (simbolo a b)
  (eval (list simbolo a b))
)

;;; Exercicio avaliacao de turmas de alunos

;; Teste: (turma-1)
;; (("Joao" "Silva" (12.5 15 8.5)) ("Ana" "Santos" (11.5 18 13.5)) ("Paulo" "Jorge" (6.5 10 7.5)) ("Elisabete" "Navarro" (12.5 15 8.5)) ("Mario" "Rodrigues" (12.5 15 8.5)))
(defun turma-1 ()
  (list (list '"Joao" '"Silva" (list 12.5 15 8.5)) (list '"Ana" '"Santos" (list 11.5 18 13.5)) (list '"Paulo" '"Jorge" (list 6.5 10 7.5)) (list '"Elisabete" '"Navarro" (list 12.5 15 8.5)) (list '"Mario" '"Rodrigues" (list 12.5 15 8.5)))
)

;; nome
(defun nome (aluno)
  (first aluno)
)

;; apelido
(defun apelido (aluno)
  (second aluno)
)

;; notas
(defun notas (aluno)
  (third aluno)
)

;; media-das-notas
;; Teste: (media-das-notas '(10 15 20))
;; 15
(defun media-das-notas (lista)
  (media (first lista) (second lista) (third lista))
)

;; media-da-turma
;; (media-da-turma 'media-das-notas (turma-1))
;; 11.666666
(defun media-da-turma(func turma)
  (apply 'media (mapcar func (mapcar #'third turma)))
)

;; percentagem-de-aprovados
;; (percentagem-aprovados (turma-1))
;; 80.0
(defun percentagem-aprovados(turma)
  (/ (* 100 (apply '+ (mapcar (lambda (n) (cond((> n 9.5) 1) (t 0))) (mapcar #'media-das-notas (mapcar #'third turma))))) (list-length turma))
)

;; lista-dos-aprovados
;; Teste: (lista-dos-aprovados (turma-1))
;; Teste avaliar-turma: (avaliar-turma 'lista-dos-aprovados (turma-1))
;; (("Joao" "Silva") ("Ana" "Santos") NIL ("Elisabete" "Navarro") ("Mario" "Rodrigues"))
(defun lista-dos-aprovados(turma)
  (mapcar (lambda (n name) (cond((> n 9.5) name) (t nil))) (mapcar #'media-das-notas (mapcar #'third turma)) (mapcar #'list (mapcar #'nome turma) (mapcar #'apelido turma)))
)

;; todos-aprovadosp
;; (todos-aprovadosp (turma-1))
;; NIL
(defun todos-aprovadosp(turma)
  ((lambda (lista) (cond ((equal (numberp (position nil lista)) T) nil) (t t))) (lista-dos-aprovados turma))
)

;;avaliar-turma
;;(("Joao" "Silva" (12.5 15 8.5)) ("Ana" "Santos" (11.5 18 13.5)) NIL ("Elisabete" "Navarro" (12.5 15 8.5)) ("Mario" "Rodrigues" (12.5 15 8.5)))
;; (avaliar-turma (turma-1) 'media-da-turma)
;; 11.6666
;; (avaliar-turma (turma-1) 'percentagem-aprovados )
;; 80.0
(defun avaliar-turma(lista &optional func)
  (cond
   ((null func) (percentagem-aprovados lista))
   (t (funcall func lista))
  )
)

;; existep
;; (existep "Joao" "Silva" (turma-1)
;; T
(defun existep(nome apelido turma)
  (cond 
   ((find t (mapcar (lambda (n a) (cond((and (equal n nome) (equal a apelido)) t) (t nil))) (mapcar #'nome turma) (mapcar #'apelido turma))) t)
   (t nil)
  )
)
