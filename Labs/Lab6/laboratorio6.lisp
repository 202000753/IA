;;;; laboratorio6.lisp
;;;; Ficha de Laboratório nº6 - O Problema das Vasilhas de Água
;;;; Autor: 

;;; Inicialização do programa
;; iniciar
(defun iniciar ()
"Permite iniciar o programa, fazendo a leitura do teclado do estado inicial e do algoritmo a utilizar para procurar a solução (neste caso a procura na profundidade ou na largura)"
  (let* ((no (cria-no (ler-vasilhas)))
         (algoritmo (ler-algoritmo))
         (profundidade (cond ((eql algoritmo 'dfs) (ler-profundidade)) (T 9999))) )
	(cond
		((equal algoritmo 'bfs) (escreve-no (funcall algoritmo no 'no-solucaop 'sucessores (operadores))))
		((equal algoritmo 'dfs) (escreve-no (funcall algoritmo no 'no-solucaop 'sucessores (operadores) profundidade)))
	)
  )
)

;;; Input - interface que permite ler os valores iniciais das vasilhas junto do utilizador.
(defun ler-no-inicial (&optional (f t))
  (read f))

(defun ler-vasilhas ()
"Permite ler do teclado o estado inicial do problema das vasilhas."
  (let ((vasilha-a (ler-vasilha "A")) (vasilha-b (ler-vasilha "B")))
    (list vasilha-a vasilha-b)
    )
)

(defun ler-vasilha (vasilha)
"Permite ler do teclado o valor inicial de uma vasilha.
A função verifica que os valores lidos pertencem ao intervale esperado para cada vasilha."
(progn
    (format t "Insere o valor da vasilha ~A ~%" vasilha)
    (let ((valor (read)))
      (cond
        ((AND (equal vasilha "A") (OR (> valor 3) (< valor 0))) (progn (format t "Valor invalido ~%") (ler-vasilha vasilha)))
        ((AND (equal vasilha "B") (OR (> valor 5) (< valor 0))) (progn (format t "Valor invalido ~%") (ler-vasilha vasilha)))
        (T valor)
      )
  )
))

;; ler-algoritmo
(defun ler-algoritmo ()
"Permite fazer a leitura do algoritmo a utilizar."
  (progn
    (format t "Que algoritmo quer usar para procurar? ~%")
    (format t "1- Procura na largura ~%")
    (format t "2- Procura na profundidade ~%")
    (let ((resposta (read)))
      (cond ((= resposta 1) 'bfs)
            (T 'dfs)))
    )
  )
;; ler-profundidade
(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite para o algoritmo dfs."
    (progn
    (format t "Qual a profundidade limite? ~%")
    (read)
    ))


;;; Output - escrita do estado do problema
;;
(defun escrever-no (no &optional (g t))
"Permite escrever um no, por defeito no ecra."
  (format g "~A" no))

 
(defun escreve-no (no)
 "Permite escrever no ecra um no do problema."
  (progn
     (format t "| A: ~a | B: ~a | G: ~a |~%" (vasilha-a-conteudo no) (vasilha-b-conteudo no) (no-profundidade no))
     (format t "Pai: ~a ~%" (no-pai no))
  ))

(defun escreve-lista-nos (lista)
  "Permite escrever no ecra uma lista de nos do problema das vasilhas, e.g. um conjunto de sucessores, a lista de abertos etc."
  (cond
   ((null lista) nil)
   (T (progn (escreve-no (car lista)) (escreve-lista-nos (cdr lista))))))


;;; Problema das vasilhas
;;; variaveis de teste e operadores
(defun no-teste ()
"Define um no teste do problema da vasilhas em que A=2, B=2, profundidade=0 e pai=NIL"
 (list '(2 2) 0 nil))

(defun operadores ()
 "Cria uma lista com todos os operadores do problema das vasilhas."
 (list 'vazar-a 'vazar-b 'encher-a 'encher-b 'transferir-a-b 'transferir-b-a))

;;; Construtor
(defun cria-no (vasilhas &optional (g 0) (pai nil))
  (list vasilhas g pai)
)


;;; Metodos seletores
;; no-estado
;; teste: (no-estado (no-teste))
;; resultado: (2 2)
(defun no-estado (no)
  (car no)
)

;; vasilha-a-conteudo
;; teste: (vasilha-a-conteudo (no-teste))
;; resultado: 2
(defun vasilha-a-conteudo (no)
  (first (no-estado no))
)

;; vasilha-b-conteudo
;; teste: (vasilha-b-conteudo (no-teste))
;; resultado: 2
(defun vasilha-b-conteudo (no)
  (second (no-estado no))
)

;; no-profundidade
;; teste: (no-profundidade (no-teste))
;; resultado: 0
(defun no-profundidade (no)
  (second no)
)

;; no-pai
;; teste: (no-pai (no-teste))
;; resultado: NIL
(defun no-pai (no)
  (third no)
)



;;; Operadores do problema
;; transferir: que permite vazar o conteudo de uma vasilha para outra ou para fora.
;; encher: para encher uma vasilha ate o topo

;; teste: (vazar-a (no-estado (no-teste)))
;; resultado: (0 2)
(defun vazar-a (no)
  (cond
   ((= (first no) 0) nil)
   (t (list '0 (second no)))
  )
)

;; teste: (vazar-b (no-estado (no-teste)))
;; resultado: (2 0)
(defun vazar-b (no)
  (cond
   ((= (second no) 0) nil)
   (t (list (first no) '0))
  )
)

;; teste: (encher-a (no-estado (no-teste)))
;; resultado: (3 2)
(defun encher-a (no)
  (cond
   ((= (first no) 3) nil)
   (t (list '3 (second no)))
  )
)

;; teste: (encher-b (no-estado (no-teste)))
;; resultado: (2 5)
(defun encher-b (no)
  (cond
   ((= (second no) 5) nil)
   (t (list (first no) '5))
  )
)


;; teste: (transferir-a-b (no-estado (no-teste)))
;; resultado: (0 4)
(defun transferir-a-b (no)
  (cond
   ((= (second no) 5) nil)
   ((> (+ (first no) (second no)) 5) (list (- (+ (first no) (second no)) 5) '5))
   (t (list '0 (+ (first no) (second no))))
  )
)

;; teste: (transferir-b-a (no-estado (no-teste)))
;; resultado: (3 1)
(defun transferir-b-a (no)
  (cond
   ((= (first no) 3) nil)
   ((> (+ (first no) (second no)) 3) (list '3 (- (+ (first no) (second no)) 3)))
   (t (list (+ (first no) (second no)) '0))
  )
)

;;; Funcoes auxiliares da procura
;;; predicado no-solucaop que verifica se um estado e final
;; teste: (no-solucaop (no-teste))
;; resultado: NIL
(defun no-solucaop (no)
  (cond
   ((or (= (vasilha-a-conteudo no) 1) (= (vasilha-b-conteudo no) 1)) t)
   (t nil)
  )
)


;;; sucessores
;; teste: (novo-sucessor (no-teste) 'encher-a)
;; resultado: ((3 2) 1 ((2 2) 0 NIL))
;; teste: (novo-sucessor (no-teste) 'transferir-a-b)
;; resultado: ((0 4) 1 ((2 2) 0 NIL))
;; teste: (novo-sucessor (cria-no '(3 5)) 'encher-a)
;; resultado: NIL
(defun novo-sucessor (no func)
  (cond 
   ((equal (funcall func (no-estado no)) NIL) NIL)
   (T (list (funcall func (no-estado no)) (+ (no-profundidade no) 1) no))
  )
)

;; teste: (sucessores (no-teste) (operadores) 'bfs)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))
;; teste: (sucessores (no-teste) (operadores) 'dfs 2)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))
(defun sucessores (no funcs algo &optional prof)
  (cond 
   ((and (equal algo 'dfs) (= (no-profundidade no) prof)) NIL)
   (T (mapcar #'(lambda (func) (novo-sucessor no func)) funcs))
  )
)


;;; Algoritmos
;; procura na largura
;; teste: (bfs (no-teste) 'no-solucaop 'sucessores (operadores) nil nil)
;; resultado: ((3 1) 1 ((2 2) 0 NIL))
(defun bfs (no solucao sucessor funcs &optional abertos fechados)
  (let* 
    ((lista-abertos (juntar-no abertos no)))
    (bfs-loop solucao sucessor funcs lista-abertos fechados)
  )
)

(defun bfs-loop (solucao sucessor funcs abertos fechados)
  (cond
   ((null abertos) nil)
   (t (let*
        ((no-n (car abertos))
        (lista-fechados (juntar-no fechados no-n))
        (lista-sucessor (funcall sucessor no-n funcs 'bfs))
        (lista-abertos (abertos-bfs (cdr abertos) (no-lista-existe lista-sucessor lista-fechados 'bfs))))
        (cond
         ((equal (no-solucao-lista lista-sucessor) 'nil) (bfs-loop solucao sucessor funcs lista-abertos lista-fechados))
         (t (no-solucao-lista lista-sucessor))
        )
      )
   )
  )
)

;; procura na profundidade
;; teste: (dfs (no-teste) 'no-solucaop 'sucessores (operadores) 10)
;; resultado: ((3 1) 1 ((2 2) 0 NIL))
(defun dfs (no solucao sucessor funcs prof &optional abertos fechados)
  (let* 
    ((lista-abertos (juntar-no abertos no)))
    (dfs-loop solucao sucessor funcs prof lista-abertos fechados)
  )
)

(defun dfs-loop (solucao sucessor funcs prof abertos fechados)
  (cond
   ((null abertos) nil)
   (t (let*
        ((no-n (car abertos))
        (lista-fechados (juntar-no fechados no-n))
        (lista-sucessor (funcall sucessor no-n funcs 'dfs prof))
        (lista-abertos (abertos-dfs (cdr abertos) (no-lista-existe lista-sucessor lista-fechados 'dfs))))
        (cond
         ((> (no-profundidade no-n) prof) (dfs-loop solucao sucessor funcs prof (cdr abertos) lista-fechados))
         ((equal (no-solucao-lista lista-sucessor) 'nil) (dfs-loop solucao sucessor funcs prof lista-abertos lista-fechados))
         (t (no-solucao-lista lista-sucessor))
        )
      )
   )
  )
)


;;abertos-bfs
(defun abertos-bfs (labertos lsucessores)
  (append labertos lsucessores)
)

;;abertos-dfs
(defun abertos-dfs (labertos lsucessores)
  (append lsucessores labertos)
)

;;juntar-no
(defun juntar-no (lista no)
  (append lista (list no))
)

;;no-existep
(defun no-existep (no lista algo)
  (cond
   ((equal algo 'bfs) (cond
                       ((null lista) nil)
                       ((equal (no-estado no) (no-estado (car lista))) t)
                       (t (no-existep no (cdr lista) 'bfs))
                      ))
   ((equal algo 'dfs) (cond
                       ((null lista) nil)
                       ((equal (no-estado no) (no-estado (car lista))) t)
                       (t (no-existep no (cdr lista) 'dfs))
                      ))
   (t nil)
  )
)

;;no-lista-existe
(defun no-lista-existe (lista-nos lista algo)
  (mapcar #'(lambda (no) (cond
                         ((equal (no-existep no lista algo) 'nil) no)
                         (t (values))
                        )) lista-nos)
)

;;no-solucao-lista
(defun no-solucao-lista (lista-nos)
  (cond
   ((null lista-nos) nil)
   ((equal (no-solucaop (car lista-nos)) 't) (car lista-nos))
   (t (no-solucao-lista (cdr lista-nos)))
  )
)