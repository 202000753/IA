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
;;(defun escrever-no (no &optional (g t))
;;"Permite escrever um no, por defeito no ecra."
;;  (format g "~A" no))

 
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
 (list '(2 2) 0 1 nil))

(defun operadores ()
 "Cria uma lista com todos os operadores do problema das vasilhas."
 (list 'vazar-a 'vazar-b 'encher-a 'encher-b 'transferir-a-b 'transferir-b-a))

;;; Construtor
(defun cria-no (vasilhas &optional (g 0) (pai nil))
  (list vasilhas g (heuristica vasilhas) pai)
)


;;; Metodos seletores
;; no-estado
;; teste: (no-estado (no-teste))
;; resultado: (2 2)
(defun no-estado (no)
  (first no)
)

;; vasilha-a-conteudo
;; teste: (vasilha-a-conteudo (no-teste))
;; resultado: 2
(defun vasilha-a-conteudo (no)
  (first (first no))
)

;; vasilha-b-conteudo
;; teste: (vasilha-b-conteudo (no-teste))
;; resultado: 2
(defun vasilha-b-conteudo (no)
  (second (first no))
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
  (fourth no)
)

(defun no-heuristica (no)
  (third no)
)

(defun no-custo (no)
  (+ (no-heuristica no) (no-profundidade no))
)

;; no-solucaop
(defun no-solucaop (no)
  (cond ((or (= (vasilha-a-conteudo no) 1) (= (vasilha-b-conteudo no) 1)) T)
        (T NIL))
)


;;; Operadores do problema
;; transferir: que permite vazar o conteudo de uma vasilha para outra ou para fora.
;; encher: para encher uma vasilha ate o topo
;; vazar-a
(defun vazar-a (estado)
  (cond ((= (first estado) 0) NIL) 
        (T (list '0 (second estado))))
)

;; vazar-b
(defun vazar-b (estado)
  (cond ((= (second estado) 0) NIL)
        (T (list (first estado) '0)))
)

;; encher-a
(defun encher-a (estado)
  (cond ((= (first estado) 3) NIL)
        (T (list '3 (second estado))))
)

;; encher-b
(defun encher-b (estado)
  (cond ((= (second estado) 5) NIL)
        (T (list (first estado) '5)))
)

;; transferir-a-b
(defun transferir-a-b (estado)
  (cond ((or (= (first estado) 0) (= (second estado) 5)) NIL)
        ((<= (+ (first estado) (second estado)) 5) (list '0 (+ (first estado) (second estado))))
        (T (list (- (+ (first estado) (second estado)) 5) '5)))
)

;; transferir-b-a
(defun transferir-b-a (estado)
  (cond ((or (= (second estado) 0) (= (first estado) 3)) NIL)
        ((<= (+ (first estado) (second estado)) 3) (list (+ (first estado) (second estado)) '0))
        (T (list '3 (- (+ (first estado) (second estado)) 3))))
)

;; teste: (vazar-a (no-estado (no-teste)))
;; resultado: (0 2)

;; teste: (vazar-b (no-estado (no-teste)))
;; resultado: (2 0)

;; teste: (encher-a (no-estado (no-teste)))
;; resultado: (3 2)

;; teste: (encher-b (no-estado (no-teste)))
;; resultado: (2 5)

;; teste: (transferir-a-b (no-estado (no-teste)))
;; resultado: (0 4)

;; teste: (transferir-b-a (no-estado (no-teste)))
;; resultado: (3 1)

;;; Funcoes auxiliares da procura
;;; predicado no-solucaop que verifica se um estado e final
;; teste: (no-solucaop (no-teste))
;; resultado: NIL


;;; sucessores
(defun novo-sucessor (no operador &optional (funcao 'heuristica))
  (cond ((equal (funcall operador (no-estado no)) NIL) NIL)
        (T (list (funcall operador (no-estado no)) (+ (no-profundidade no) 1) (funcall funcao (no-estado no)) no)))
)

;; teste: (novo-sucessor (no-teste) 'encher-a)
;; resultado: ((3 2) 1 ((2 2) 0 NIL))
;; teste: (novo-sucessor (no-teste) 'transferir-a-b)
;; resultado: ((0 4) 1 ((2 2) 0 NIL))
;; teste: (novo-sucessor (cria-no '(3 5)) 'encher-a)
;; resultado: NIL

(defun sucessores (no operadores algoritmo &optional (funcao 'heuristica) profundidade)
  (cond ((and (equal algoritmo 'dfs) (= (no-profundidade no) profundidade)) NIL)
        (T (mapcar #'(lambda (operador) (novo-sucessor no operador funcao)) operadores)))
)

(defun ordenar-nos (nos)
  (sort nos #'< :key #'no-custo)
)

(defun heuristica (estado)
  (let* ((vA (first estado))
         (vB (second estado)))
    (cond
     ((or (= vA 1) (= vB 1)) 0)
     ((and (= vA vB) (/= vA 1)) 1)
     (T 2)
)
)
)

(defun colocar-sucessores-em-abertos (abertos nosSucessores)
  (ordenar-nos (append abertos nosSucessores))
)



;; teste: (sucessores (no-teste) (operadores) 'bfs)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))
;; teste: (sucessores (no-teste) (operadores) 'dfs 2)
;; resultado: (((0 2) 1 ((2 2) 0 NIL)) ((2 0) 1 ((2 2) 0 NIL)) ((3 2) 1 ((2 2) 0 NIL)) ((2 5) 1 ((2 2) 0 NIL)) ((0 4) 1 ((2 2) 0 NIL)) ((3 1) 1 ((2 2) 0 NIL)))

;; COPIAR
(defun abertos-bfs (lista-abertos lista-sucessores)
  (append lista-abertos lista-sucessores)
)

;; COPIAR
(defun abertos-dfs (lista-abertos lista-sucessores)
  (append lista-sucessores lista-abertos)
)

;; COPIAR
(defun juntar-no (lista no)
  (append lista (list no))
)

(defun no-existep (no lista algoritmo)
  (cond ((equal algoritmo 'bfs) 
         (cond ((NULL lista) NIL)
               ((equal (no-estado no) (no-estado (car lista))) T)
               (T (no-existep no (cdr lista) 'bfs))))
        ((equal algoritmo 'dfs) 
         (cond ((NULL lista) NIL)
               ((equal (no-estado no) (no-estado (car lista))) T)
               (T (no-existep no (cdr lista) 'dfs))))
        (T NIL))
)

;; COPIAR
(defun no-existe-lista (lista-nos lista-verificar algoritmo)
  (mapcar #'(lambda (single-no) (cond ((equal (no-existep single-no lista-verificar algoritmo) 'NIL) single-no)
                                      (T (values)))) lista-nos)
)

;; COPIAR
(defun no-solucao-lista (lista)
  (cond ((NULL lista) NIL)
        ((equal (no-solucaop (car lista)) 'T) (car lista))
        (T (no-solucao-lista (cdr lista))))
)

;; COPIAR
(defun bfs-loop (funcao-a-testar funcao-sucessores lista-operadores abertos fechados)
  (cond ((NULL abertos) NIL)
        (T (let* ((no-n (car abertos))
                   (novo-fechados (juntar-no fechados no-n))
                   (novo-sucessores (funcall funcao-sucessores no-n lista-operadores 'bfs))
                   (novo-abertos (abertos-bfs (cdr abertos) (no-existe-lista novo-sucessores novo-fechados 'bfs))))
              (cond ((equal (no-solucao-lista novo-sucessores) 'NIL) (bfs-loop funcao-a-testar funcao-sucessores lista-operadores novo-abertos novo-fechados))
                    (T (no-solucao-lista novo-sucessores))))
            ))
)

;; COPIAR
(defun bfs (no funcao-a-testar funcao-sucessores lista-operadores &optional abertos fechados)
  (let* ((novo-abertos (juntar-no abertos no)))
        (bfs-loop funcao-a-testar funcao-sucessores lista-operadores novo-abertos fechados))
)

;; COPIAR
(defun dfs-loop (funcao-a-testar funcao-sucessores lista-operadores profundidade-max abertos fechados)
  (cond ((NULL abertos) NIL)
        (T (let* ((no-n (car abertos))
                  (novo-fechados (juntar-no fechados no-n))
                  (novo-sucessores (funcall funcao-sucessores no-n lista-operadores 'dfs profundidade-max))
                  (novo-abertos (abertos-dfs (cdr abertos) (no-existe-lista novo-sucessores novo-fechados 'dfs))))
             (cond ((> (no-profundidade no-n) profundidade-max) (dfs-loop funcao-a-testar funcao-sucessores lista-operadores profundidade-max (cdr abertos) novo-fechados))
                   ((equal (no-solucao-lista novo-sucessores) 'NIL) (dfs-loop funcao-a-testar funcao-sucessores lista-operadores profundidade-max novo-abertos novo-fechados))
                   (T (no-solucao-lista novo-sucessores))))
           ))
)

;; COPIAR
(defun dfs (no funcao-a-testar funcao-sucessores lista-operadores profundidade-max &optional abertos fechados)
  (let* ((novo-abertos (juntar-no abertos no)))
        (dfs-loop funcao-a-testar funcao-sucessores lista-operadores profundidade-max novo-abertos fechados))
)

;;; Algoritmos
;; procura na largura
;; teste: (bfs (no-teste) 'no-solucaop 'sucessores (operadores) nil nil)
;; resultado: ((3 1) 1 ((2 2) 0 NIL))

;; procura na profundidade
;; teste: (dfs (no-teste) 'no-solucaop 'sucessores (operadores) 10)
;; resultado: ((3 1) 1 ((2 2) 0 NIL))

;;a*
;;(a* (no-teste) 'no-solucaop 'sucessores (operadores) 'heuristica)
(defun a* (no problema nosSucessores operadoress funcao  &optional (gerados 0) (expandidos 0))
  (let* ((novos-abertos (juntar-no '() no))
         (fehados '())
         (solucao (a*-loop problema nosSucessores operadoress novos-abertos fehados funcao gerados expandidos))
         (no (first solucao))
         (ngerados (second solucao))
         (nexpandidos (third solucao))
        )
    (mostrar-solucao no ngerados nexpandidos)
    (escrever-no no ngerados nexpandidos)
)
)

(defun a*-loop (problema nosSucessores operadoress abertos fechados funcao gerados expandidos)
  (cond
   ((NULL abertos) NIL)
   (T (let* ((no-n (car (ordenar-nos abertos)))
             (novos-fechados (remove nil (juntar-no fechados no-n)))
             (novos-sucessores (remove nil (funcall nosSucessores no-n operadoress 'a* funcao)))
             (novos-abertos (colocar-sucessores-em-abertos (cdr abertos) (no-existe-lista novos-sucessores novos-fechados 'a*)))
             (ngerados (+ gerados (list-length novos-sucessores)))
             (nexpandidos (1+ expandidos))
            )
        (cond
         ((equal (no-solucao-lista novos-abertos) NIL) (a*-loop problema nosSucessores operadoress novos-abertos novos-fechados funcao ngerados nexpandidos))
         (T (list (no-solucao-lista novos-abertos) ngerados nexpandidos))
        )
      )
   )
  )
)

;;gerados +6
;;expandidos +1
(defun mostrar-solucao (no gerados expandidos)
  (cond 
   ((NULL (no-pai no)) (format t "Nos gerados: ~A, Nos expandidos: ~A ~%" gerados expandidos) (format t "Estado: ~A, Profundidade: ~A, Heuristica: ~A, Custo: ~A ~%" (no-estado no) (no-profundidade no) (no-heuristica no) (no-custo no)))
   (T (mostrar-solucao (no-pai no) gerados expandidos) (format t "Estado: ~A, Profundidade: ~A, Heuristica: ~A, Custo: ~A ~%" (no-estado no) (no-profundidade no) (no-heuristica no) (no-custo no)))
  )
)

;;escrever-no
(defun escrever-no (no gerados expandidos)
  (cond 
   ((NULL (no-pai no)) (with-open-file (str "C:/Users/nunor/Desktop/IA/Labs/Lab89/log.dat" :direction :output :if-exists :supersede :if-does-not-exist :create)
                         (format str "Nos gerados: ~A, Nos expandidos: ~A ~%" gerados expandidos)
                         (format str "Estado: ~A, Profundidade: ~A, Heuristica: ~A, Custo: ~A ~%" (no-estado no) (no-profundidade no) (no-heuristica no) (no-custo no))
                       )
   )
   (T (escrever-no (no-pai no) gerados expandidos) (with-open-file (str "C:/Users/nunor/Desktop/IA/Labs/Lab89/log.dat" :direction :output :if-exists :append :if-does-not-exist :create)
                                                         (format str "Estado: ~A, Profundidade: ~A, Heuristica: ~A, Custo: ~A ~%" (no-estado no) (no-profundidade no) (no-heuristica no) (no-custo no))
                                                       )
   )
  )
)

;;log.dat


;;ler-ficheiro
(defun ler-ficheiro ()
  (let 
    ((in (open "C:/Users/nunor/Desktop/IA/Labs/Lab89/problema.dat")))
    (format t "~A~%" (read-line in))
    (close in)
  )
)