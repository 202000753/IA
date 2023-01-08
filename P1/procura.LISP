;;;; procura.lisp
;;;; Algoritmos de procura
;;;; Autores: Nuno Reis, Vitor Nunes

;(operadores)
(defun operadores ()
  (list 'arco-horizontal 'arco-vertical)
)

;(criar-no (tabuleiro-teste) 3)
(defun criar-no (arcos ncaixasSolucao &optional (p 0) (pai nil))
  (list arcos ncaixasSolucao p (heuristica arcos ncaixasSolucao) pai)
)

;(no-estado (criar-no (tabuleiro-teste) 3))
(defun no-estado (no)
  (first no)
)

;(n-caixas-solucao (criar-no (tabuleiro-teste) 3))
(defun n-caixas-solucao (no)
  (second no)
)

;(no-profundidade (criar-no (tabuleiro-teste) 3))
(defun no-profundidade (no)
  (third no)
)

;(no-heuristica (criar-no (tabuleiro-teste) 3))
(defun no-heuristica (no)
  (fourth no)
)

;(no-pai (criar-no (tabuleiro-teste) 3))
(defun no-pai (no)
  (fifth no)
)

;(heuristica (tabuleiro-teste) 3)
(defun heuristica (arcos n)
  (- n (n-caixas-fechadas (get-arcos-horizontais arcos) (get-arcos-verticais arcos) (1- (list-length (get-arcos-horizontais arcos))) (1- (list-length (get-arcos-verticais arcos)))))
)

(defun heuristica2 (arcos n)
  (- (* n 4) (n-cantos (get-arcos-horizontais arcos) (get-arcos-verticais arcos) (list-length (get-arcos-horizontais arcos)) (1- (list-length (get-arcos-verticais arcos)))))
)

;(no-custo (tabuleiro-teste) 3)
(defun no-custo (no)
  (+ (no-heuristica no) (no-profundidade no))
)

;(no-solucaop (criar-no (tabuleiro-teste) 3))
(defun no-solucaop (no)
  (cond
   ((= (n-caixas-fechadas (get-arcos-horizontais (no-estado no)) (get-arcos-verticais (no-estado no)) (1- (list-length (get-arcos-horizontais (no-estado no)))) (1- (list-length (get-arcos-verticais (no-estado no))))) (n-caixas-solucao no)) T)
   (T NIL)
  )
)

;(novo-sucessor (criar-no (tabuleiro-teste) 3) 'arco-horizontal 1 1)
(defun novo-sucessor (no operador linha coluna &optional (funcao 'heuristica))
  (let
      (
       (estado (funcall operador linha coluna (no-estado no)))
      )
    (cond 
     ((equal estado NIL) NIL)
     (T(list estado (n-caixas-solucao no) (1+ (no-profundidade no)) (funcall funcao (no-estado no) (n-caixas-solucao no)) no))
    )
  )
)

;(mapcar #'(lambda (no) (format t "~A~%" no)) (gerar-horizontais (criar-no (tabuleiro-teste) 3) 'heuristica (list-length (get-arcos-horizontais (tabuleiro-teste))) (1- (list-length (get-arcos-verticais (tabuleiro-teste))))))
;(gerar-horizontais (criar-no (tabuleiro-teste) 3) 'heuristica (list-length (get-arcos-horizontais (tabuleiro-teste))) (1- (list-length (get-arcos-verticais (tabuleiro-teste)))))
(defun gerar-horizontais (no heuristica n-horizontais n-verticais &optional (nh 1) (nv 1) (lista '()))
  (cond
   ((and (= nh n-horizontais) (= nv n-verticais)) (append (cons (novo-sucessor no 'arco-horizontal nh nv heuristica) nil)))
   (t
    (append (cons (novo-sucessor no 'arco-horizontal nh nv heuristica) nil)
     (cond 
      ((/= nv n-verticais) (gerar-horizontais no heuristica n-horizontais n-verticais nh (1+ nv) lista))
      (t (gerar-horizontais no heuristica n-horizontais n-verticais (1+ nh) 1 lista))
     )
    )
   )
  )
)


;(mapcar #'(lambda (no) (format t "~A~%" no)) (gerar-verticais (criar-no (tabuleiro-teste) 3) 'heuristica (1- (list-length (get-arcos-horizontais (tabuleiro-teste)))) (list-length (get-arcos-verticais (tabuleiro-teste)))))
;(gerar-verticais (criar-no (tabuleiro-teste) 3) 'heuristica (list-length (1- (get-arcos-horizontais (tabuleiro-teste)))) (list-length (get-arcos-verticais (tabuleiro-teste))))
(defun gerar-verticais (no heuristica n-horizontais n-verticais &optional (nh 1) (nv 1) (lista '()))
  (cond
   ((and (= nh n-horizontais) (= nv n-verticais)) (append (cons (novo-sucessor no 'arco-vertical nv nh heuristica) nil)))
   (t
    (append (cons (novo-sucessor no 'arco-vertical nv nh heuristica) nil)
     (cond 
      ((/= nh n-horizontais) (gerar-verticais no heuristica n-horizontais n-verticais (1+ nh) nv lista))
      (t (gerar-verticais no heuristica n-horizontais n-verticais 1 (1+ nv) lista))
     )
    )
   )
  )
)

;(mapcar #'(lambda (no) (format t "~A~%" no))(sucessores (criar-no (tabuleiro-teste) 3) (operadores) 'bfs))
;(sucessores (criar-no (tabuleiro-teste) 3) (operadores) 'bfs)
(defun sucessores (no operadores algoritmo &optional (funcao 'heuristica) (profundidade 100))
  (let
      (
       (lista '())
      )
    (cond
     ((and (equal algoritmo 'dfs) (= (no-profundidade no) profundidade)) NIL)
     (T (apply 'append lista (mapcar #'(lambda (operador) (cond
                                                           ((eq operador 'arco-horizontal) (gerar-horizontais no funcao (list-length (get-arcos-horizontais (no-estado no))) (1- (list-length (get-arcos-verticais (no-estado no))))))
                                                           ((eq operador 'arco-vertical) (gerar-verticais no funcao (1- (list-length (get-arcos-horizontais (no-estado no)))) (list-length (get-arcos-verticais (no-estado no)))))
                                                           (t nil)
                                                           )) operadores)))
     )
  )
)

(defun juntar-no (lista no)
  (append lista (list no))
)

(defun abertos-bfs (lista-abertos lista-sucessores)
  (append lista-abertos lista-sucessores)
)

(defun abertos-dfs (lista-abertos lista-sucessores)
  (append lista-sucessores lista-abertos)
)

(defun no-existep (no lista algoritmo)
  (cond
   ((equal algoritmo 'bfs) (cond
                            ((NULL lista) NIL)
                            ((equal (no-estado no) (no-estado (car lista))) T)
                            (T (no-existep no (cdr lista) 'bfs))
                           ))
   ((equal algoritmo 'dfs) (cond
                            ((NULL lista) NIL)
                            ((equal (no-estado no) (no-estado (car lista))) T)
                            (T (no-existep no (cdr lista) 'dfs))
                           ))
   (T NIL)
  )
)

(defun no-existe-lista (lista-nos lista-verificar algoritmo)
  (mapcar #'(lambda (single-no) (cond
                                 (
                                  (equal (no-existep single-no lista-verificar algoritmo) 'NIL) single-no)
                                  (T (values))
                                 )) lista-nos)
)

(defun no-solucao-lista (lista)
  (cond ((NULL lista) NIL)
        ((null (car lista)) (no-solucao-lista (cdr lista)))
        ((equal (no-solucaop (car lista)) 'T) (car lista))
        (T (no-solucao-lista (cdr lista))))
)

(defun ordenar-nos (nos)
  (sort nos #'< :key #'no-custo)
)

(defun colocar-sucessores-em-abertos (abertos nosSucessores)
  (ordenar-nos (append abertos nosSucessores))
)

;(bfs 'no-solucaop 'sucessores (list no) '())
;(bfs 'no-solucaop 'sucessores (list (criar-no (tabuleiro-teste) 3)) '())
(defun bfs (fucao-de-teste funcao-sucessores abertos fechados &optional (gerados 0) (expandidos 0))
  (cond
   ((null abertos) nil)
   (t (cond
       ((null (car abertos)) (bfs fucao-de-teste funcao-sucessores (cdr abertos) fechados))
       (t 
           (let*
              (
               (no (car abertos))
               (novo-fechados (remove nil (juntar-no fechados no)))
               (novo-sucessores (remove nil (funcall funcao-sucessores no (operadores) 'bfs)))
               (novo-abertos (abertos-bfs (cdr abertos) (no-existe-lista novo-sucessores novo-fechados 'bfs)))
               (novo-gerados (+ gerados (list-length novo-sucessores)))
               (novo-expandidos (+ expandidos 1))
              );(format t "~A~%" no)
             ;(format t "novo-fechados~%")
             ;(mapcar #'(lambda (no) (format t "~A~%" no)) novo-fechados)
             ;(format t "novo-sucessores~%")
             ;(mapcar #'(lambda (no) (format t "~A~%" no)) novo-sucessores)
             ;(format t "novo-abertos~%")
             ;(mapcar #'(lambda (no) (format t "~A~%" no)) novo-abertos)
             (cond
              ((equal (no-solucao-lista novo-sucessores) 'nil) (bfs fucao-de-teste funcao-sucessores novo-abertos novo-fechados novo-gerados novo-expandidos))
              (t (list (no-solucao-lista novo-sucessores) (list novo-gerados novo-expandidos)))
             )
          )
       )
      )
   )
  )
)

;(dfs 'no-solucaop 'sucessores profundidade (list no) '())
;(dfs 'no-solucaop 'sucessores 10 (list (criar-no (tabuleiro-teste) 3)) '())
(defun dfs (fucao-de-teste funcao-sucessores profundidade abertos fechados &optional (gerados 0) (expandidos 0))
  (cond
   ((null abertos) nil)
   (t (cond
       ((null (car abertos)) (bfs fucao-de-teste funcao-sucessores (cdr abertos) fechados))
       (t 
           (let*
              (
               (no (car abertos))
               (novo-fechados (remove nil (juntar-no fechados no)))
               (novo-sucessores (remove nil (funcall funcao-sucessores no (operadores) 'dfs 'heuristica profundidade)))
               (novo-abertos (abertos-dfs (cdr abertos) (no-existe-lista novo-sucessores novo-fechados 'dfs)))
               (novo-gerados (+ gerados (list-length novo-sucessores)))
               (novo-expandidos (+ expandidos 1))
              );(format t "~A~%" no)
             ;(format t "novo-fechados~%")
             ;(mapcar #'(lambda (no) (format t "~A~%" no)) novo-fechados)
             ;(format t "novo-sucessores~%")
             ;(mapcar #'(lambda (no) (format t "~A~%" no)) novo-sucessores)
             ;(format t "novo-abertos~%")
             ;(mapcar #'(lambda (no) (format t "~A~%" no)) novo-abertos)
             (cond
              ((= (no-profundidade no) (1- profundidade)) (dfs fucao-de-teste funcao-sucessores profundidade (cdr abertos) novo-fechados novo-gerados novo-expandidos))
              ((equal (no-solucao-lista novo-sucessores) 'nil) (dfs fucao-de-teste funcao-sucessores profundidade novo-abertos novo-fechados novo-gerados novo-expandidos))
              (t (list (no-solucao-lista novo-sucessores) (list novo-gerados novo-expandidos)))
             )
          )
       )
      )
   )
  )
)

;(a* 'no-solucaop 'sucessores (escolher-nome-heuristica tabuleiro) (list (criar-no (first tabuleiro) (second tabuleiro))) '())
;(a* 'no-solucaop 'sucessores heuristica (list no) '())
;(a* 'no-solucaop 'sucessores 'heuristica (list (criar-no (tabuleiro-teste) 3)) '())
(defun a* (fucao-de-teste funcao-sucessores heuristica abertos fechados &optional (gerados 0) (expandidos 0))
  (cond
   ((null abertos) nil)
   (t (cond
       ((null (car abertos)) (a* fucao-de-teste funcao-sucessores heuristica (cdr abertos) fechados))
       (t 
           (let*
              (
               (no (car (ordenar-nos abertos)))
               (novo-fechados (remove nil (juntar-no fechados no)))
               (novo-sucessores (remove nil (funcall funcao-sucessores no (operadores) 'a* heuristica)))
               (novo-abertos (colocar-sucessores-em-abertos (cdr abertos) (no-existe-lista novo-sucessores novo-fechados 'a*)))
               (novo-gerados (+ gerados (list-length novo-sucessores)))
               (novo-expandidos (+ expandidos 1))
              );(format t "~%~A~%" no)
             ;(format t "novo-fechados~%")
             ;(mapcar #'(lambda (no) (format t "~A~%" no)) novo-fechados)
             ;(format t "novo-sucessores~%")
             ;(mapcar #'(lambda (no) (format t "~A~%" no)) novo-sucessores)
             ;(format t "novo-abertos~%")
             ;(mapcar #'(lambda (no) (format t "~A~%" no)) novo-abertos)
             (cond
              ((equal (no-solucao-lista novo-sucessores) 'nil) (a* fucao-de-teste funcao-sucessores heuristica novo-abertos novo-fechados novo-gerados novo-expandidos))
              (t (list (no-solucao-lista novo-sucessores) (list novo-gerados novo-expandidos)))
             )
          )
       )
      )
   )
  )
)

(defun tamanho-caminho (no)
  (cond
   ((null no) 0)
   ((NULL (no-pai no)) 1)
   (t (1+ (tamanho-caminho (no-pai no))))
  )
)