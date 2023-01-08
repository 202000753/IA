;;;; projeto.lisp
;;;; Interação com o utilizador e, escrita e leitura de ficheiros
;;;; Autores: Nuno Reis, Vitor Nunes

;;; Inicializaçãoo do programa
;; iniciar
(defun iniciar ()
"Permite iniciar o programa"
  ;(progn
  ;  (format t "Dots and Boxes~%")
  ;  (format t "Escolha uma opção~%")
  ;  (format t "0- Sair~%")
  ;  (format t "1- Novo Jogo~%")
  ;  (format t "2- Jogar Nivéis Do Ficheiro~%")
  ;  (let ((resposta (read)))
  ;    (cond 
  ;     ((and (equal (equal resposta 0) nil) (equal (equal resposta 1) nil) (equal (equal resposta 2) nil)) (progn (format t "Opção Invalida ~%") (iniciar)))
  ;     (t (cond
  ;             ((= resposta 0) (format t "A terminar...~%"))
  ;             ((= resposta 1) (novo-jogo))
  ;             (t (escolher-nivel))
  ;         )
  ;     )
  ;    )
  ;  )
  ;)
  (escolher-nivel)
)

;; novo-jogo
(defun novo-jogo ()
"Permite iniciar um novo jogo"
  '(
    ((0 0 0) (0 0 0) (0 0 0) (0 0 0))
    ((0 0 0) (0 0 0) (0 0 0) (0 0 0))
   )
)

;; escolher-nivel
(defun escolher-nivel ()
"Permite jogar um nivél existente"
  (let 
      (
       (problemas (get-problemas))
       (opcoes (mapcar #'first (get-problemas)))
      )
    (progn
      (format t "~%Nivéis~%")
      (format t "Escolha uma opção~%")
      (format t "0- Terminar~%")
      (apresentar-opcoes problemas)
      (let 
          (
           (resposta (read))
          )
        (cond 
         ((and (equal (equal resposta 0) nil) (eq (operacao-valida opcoes resposta) nil)) (progn (format t "Opção Invalida ~%") (escolher-nivel)))
         ((equal resposta 0) (format t "A terminar...~%") nil)
         (t (escolher-algoritmo (escolher-tabuleiro problemas resposta)))
        )
      )
    )
  )
)

;; apresentar-opcoes
(defun apresentar-opcoes (problemas)
  (cond
   ((null problemas) nil)
   (t (format t "Nivél - ~A) objetivo - ~A~%" (first (car problemas)) (third (car problemas))) (apresentar-opcoes (cdr problemas)))
  )
)

;;operacao-valida
(defun operacao-valida (opcoes resposta)
  (cond
   ((null opcoes) nil)
   ((equal resposta (car opcoes)) t)
   (t (operacao-valida (cdr opcoes) resposta))
  )
)

;;escolher-tabuleiro
(defun escolher-tabuleiro (problemas resposta)
  (cond
   ((null problemas) nil)
   ((equal resposta (first (car problemas))) (list (second (car problemas)) (third (car problemas))))
   (t (escolher-tabuleiro (cdr problemas) resposta))
  )
)

;;escolher-algoritmo
(defun escolher-algoritmo (tabuleiro)
  (progn
      (format t "~%Algoritmos~%")
      (format t "Escolha uma opção~%")
      (format t "0- Voltar atrás~%")
      (format t "1- Breadth-First Search~%")
      (format t "2- Depth-First Search~%")
      (format t "3- A*~%")
      (let 
          (
           (resposta (read))
          )
        (cond 
         ((and (equal (equal resposta 0) nil) (equal (equal resposta 1) nil) (equal (equal resposta 2) nil) (equal (equal resposta 3) nil)) (progn (format t "Opção Invalida ~%") (escolher-algoritmo tabuleiro)))
         ((equal resposta 0) (escolher-nivel) nil)
         ((equal resposta 1) (mostrar-solucao (bfs 'no-solucaop 'sucessores (list (criar-no (first tabuleiro) (second tabuleiro))) '())) (iniciar))
         ((equal resposta 2) (mostrar-solucao (dfs 'no-solucaop 'sucessores (escolher-profundidade tabuleiro) (list (criar-no (first tabuleiro) (second tabuleiro))) '())) (iniciar))
         (t (mostrar-solucao (a* 'no-solucaop 'sucessores (escolher-nome-heuristica tabuleiro) (list (criar-no (first tabuleiro) (second tabuleiro))) '())) (iniciar))
        )
      )
  )
)

;;escolher-profundidade
(defun escolher-profundidade (tabuleiro)
  (progn
    (format t "Qual a profundidade limite?~%")
    (let 
        (
         (resposta (read))
        )
      (cond 
       ((<= resposta 0) (progn (format t "Opção Invalida ~%") (escolher-algoritmo tabuleiro)))
       (T resposta)
      )
    )
  )
)

;;escolher-nome-heuristica
(defun escolher-nome-heuristica (tabuleiro)
  (progn
      (format t "~%Heuristicas~%")
      (format t "Escolha uma opção~%")
      (format t "0- Voltar atrás~%")
      (format t "1- Diferença entre o numero de caixas fechadas e o numero esperado de caixas fechadas~%")
      (format t "2- Diferença entre o numero de cantos do tabuleiro e o numero de cantos esperados(numero esperado de caixas fechadas * 4)~%")
      (let 
          (
           (resposta (read))
          )
        (cond 
         ((and (equal (equal resposta 0) nil) (equal (equal resposta 1) nil) (equal (equal resposta 2) nil)) (progn (format t "Opção Invalida ~%") (escolher-nome-heuristica tabuleiro)))
         ((equal resposta 0) (escolher-algoritmo tabuleiro) nil)
         ((equal resposta 1) 'heuristica)
         (t 'heuristica2)
        )
      )
  )
)

;(mostrar-solucao (bfs 'no-solucaop 'sucessores (criar-no (tabuleiro-teste) 3)) '())
(defun mostrar-solucao (no)
  (cond 
   ((null no) (format t "Impossivel"))
   ((= (list-length no) 2) (escrever-no (first no) (first (second no)) (second (second no))) (mostrar-solucao (no-pai (first no))) (format t "Estado: ~A, Profundidade: ~A, Heuristica: ~A, Custo: ~A ~%" (no-estado (first no)) (no-profundidade (first no)) (no-heuristica (first no)) (no-custo (first no))) (format t "Nós Gerados ~A, Nós Expandidos: ~A, Penetrância: ~A~%" (first (second no)) (second (second no)) (/ (tamanho-caminho (first no)) (first (second no)))))
   ((NULL (no-pai no)) (format t "Estado: ~A, Profundidade: ~A, Heuristica: ~A, Custo: ~A ~%" (no-estado no) (no-profundidade no) (no-heuristica no) (no-custo no)))
   (T (mostrar-solucao (no-pai no)) (format t "Estado: ~A, Profundidade: ~A, Heuristica: ~A, Custo: ~A ~%" (no-estado no) (no-profundidade no) (no-heuristica no) (no-custo no)))
  )
)

(defun escrever-no (no gerados expandidos)
  (cond 
   ((NULL (no-pai no)) (with-open-file (str "C:/Users/User/Documents/IA_ProjetoP1/log.dat" :direction :output :if-exists :append :if-does-not-exist :create)
                         (format str "~%~%Nos gerados: ~A, Nos expandidos: ~A, Penetrância: ~A~%" gerados expandidos (/ (tamanho-caminho no) gerados))
                         (format str "Estado: ~A, Profundidade: ~A, Heuristica: ~A, Custo: ~A ~%" (no-estado no) (no-profundidade no) (no-heuristica no) (no-custo no))
                       )
   )
   (T (escrever-no (no-pai no) gerados expandidos) (with-open-file (str "C:/Users/User/Documents/IA_ProjetoP1/log.dat" :direction :output :if-exists :append :if-does-not-exist :create)
                                                         (format str "Estado: ~A, Profundidade: ~A, Heuristica: ~A, Custo: ~A ~%" (no-estado no) (no-profundidade no) (no-heuristica no) (no-custo no))
                                                       )
   )
  )
)

;; get-problemas
;; (get-problemas)
(defun get-problemas (&optional file (problemas '()))
  (cond
   ((null file) (with-open-file (open "C:/Users/nunor/Desktop/problemas.dat" :direction :input :if-does-not-exist nil)
                  (get-problemas open (cons (read open) nil))
                )
   )
   (t (let
          (
           (line (read file nil 'eof))
          )
        (cond
         ((eq line 'eof) problemas)
         (t (get-problemas file (append problemas (cons line nil))))
        )
      )
   )
  )
)

;; (get-problemas1)
(defun get-problemas1 ()
  (with-open-file (open "C:/Users/nunor/Desktop/problemas1.dat" :direction :input :if-does-not-exist nil)
    (read open)
  )
)