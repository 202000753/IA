;;;; algoritmo.lisp
;;;; Algorito Alfa-Beta
;;;; Autores: Nuno Reis, Vitor Nunes

(defun operadores ()
  (list 'arco-horizontal 'arco-vertical)
)

(defun estado-tabuleiro (estado)
  (first estado)
)

(defun estado-pontuacoes (estado)
  (second estado)
)

(defun no-estado (estado)
  (first estado)
)

(defun no-prof (estado)
  (second estado)
)

(defun no-utilidade (no)
  (third no)
)

(defun no-pai (no)
  (fourth no)
)

(defun alfabeta (no profundidade &optional (alfa (- 0 999)) (beta 999))
  (let*
      (
       (tabuleiro (estado-tabuleiro (no-estado no)))
       (tipo-jogador (cond ((equal (evenp (no-prof no)) t) *computador*) (t *jogador*)))
       (sucessores-no (sort (remove nil(sucessores-alfabeta no tipo-jogador)) #'> :key #'no-utilidade))
      )
    (format t "Nó ~a~%" no)
    (cond
     ((= tipo-jogador *computador*) (format t "Max~%"))
     ((= tipo-jogador *jogador*) (format t "Min~%"))
    )
    (cond
     ((or (equal (tabuleiro-preenchidop tabuleiro) t) (= profundidade (no-prof no))) (list (no-utilidade no) no))
     ((= tipo-jogador *computador*) (list (alfabeta-max no profundidade alfa beta sucessores-no) no))
     ((= tipo-jogador *jogador*) (list (alfabeta-min no profundidade alfa beta sucessores-no) no))
    )
  )
)

(defun alfabeta-max (no profundidade alfa beta sucessores-no)
  (let
      (
       (new-alfa (let
                     (
                      (aa (alfabeta (car sucessores-no) profundidade alfa beta))
                     )
                   (cond
                    ((< alfa (first aa)) (first aa))
                    (t alfa)
                   )))
      )
    (format t "alfabeta-max ~a~%" new-alfa)
    (cond
     ((> new-alfa beta) (list beta no))
     ((equal (cdr sucessores-no) nil) (list new-alfa no))
     (t (list (alfabeta-max no profundidade new-alfa beta (cdr sucessores-no)) no))
    )
   )
)

(defun alfabeta-min (no profundidade alfa beta sucessores-no)
  (let
      (
       (new-beta (let
                     (
                      (aa (alfabeta (car sucessores-no) profundidade alfa beta))
                     )
                   (cond
                    ((> beta (first aa)) (first aa))
                    (t beta)
                   )))
      )
    (format t "alfabeta-min ~a~%" new-beta)
    (cond
     ((> alfa new-beta) (list alfa no))
     ((equal (cdr sucessores-no) nil) (list new-beta no))
     (t (list (alfabeta-max no profundidade alfa new-beta (cdr sucessores-no)) no))
    )
   )
)

(defun sucessores-alfabeta (no jogador)
  (let
      (
       (lista '())
      )
    (apply 'append lista (mapcar #'(lambda (operador) (cond
                                                       ((eq operador 'arco-horizontal) (gerar-horizontais-alfabeta no jogador (list-length (get-arcos-horizontais (estado-tabuleiro (no-estado no)))) (1- (list-length (get-arcos-verticais (estado-tabuleiro (no-estado no)))))))
                                                       ((eq operador 'arco-vertical) (gerar-verticais-alfabeta no jogador (1- (list-length (get-arcos-horizontais (estado-tabuleiro (no-estado no))))) (list-length (get-arcos-verticais (estado-tabuleiro (no-estado no))))))
                                                       (t nil)
                                                      )) (operadores))
    )
  )
)

(defun gerar-horizontais-alfabeta (no jogador n-horizontais n-verticais &optional (nh 1) (nv 1) (lista '()))
  (cond
   ((and (= nh n-horizontais) (= nv n-verticais)) (append (cons (novo-sucessor-alfabeta no jogador 'arco-horizontal nh nv) nil)))
   (t
    (append (cons (novo-sucessor-alfabeta no jogador 'arco-horizontal nh nv) nil)
     (cond 
      ((/= nv n-verticais) (gerar-horizontais-alfabeta no jogador n-horizontais n-verticais nh (1+ nv) lista))
      (t (gerar-horizontais-alfabeta no jogador n-horizontais n-verticais (1+ nh) 1 lista))
     )
    )
   )
  )
)

(defun gerar-verticais-alfabeta (no jogador n-horizontais n-verticais &optional (nh 1) (nv 1) (lista '()))
  (cond
   ((and (= nh n-horizontais) (= nv n-verticais)) (append (cons (novo-sucessor-alfabeta no jogador 'arco-vertical nv nh) nil)))
   (t
    (append (cons (novo-sucessor-alfabeta no jogador 'arco-vertical nv nh) nil)
     (cond 
      ((/= nh n-horizontais) (gerar-verticais-alfabeta no jogador n-horizontais n-verticais (1+ nh) nv lista))
      (t (gerar-verticais-alfabeta no jogador n-horizontais n-verticais 1 (1+ nv) lista))
     )
    )
   )
  )
)

(defun novo-sucessor-alfabeta (no jogador operador linha coluna)
  (let*
      (
       (tabuleiro (estado-tabuleiro (no-estado no)))
       (caixas-fechadas-antes (n-caixas-fechadas (get-arcos-horizontais tabuleiro) (get-arcos-verticais tabuleiro) (1- (list-length (get-arcos-horizontais tabuleiro))) (1- (list-length (get-arcos-verticais tabuleiro)))))
       (tabuleiro-novo (funcall operador linha coluna (estado-tabuleiro (no-estado no)) jogador))
       (caixas-fechadas-depois (cond
                                ((equal tabuleiro-novo nil) 0)
                                (t (n-caixas-fechadas (get-arcos-horizontais tabuleiro-novo) (get-arcos-verticais tabuleiro-novo) (1- (list-length (get-arcos-horizontais tabuleiro-novo))) (1- (list-length (get-arcos-verticais tabuleiro-novo)))))
                               ))
       (jogador1 (cond
                  ((and (= jogador 1) (< caixas-fechadas-antes caixas-fechadas-depois)) (+ (first (estado-pontuacoes (no-estado no))) (- caixas-fechadas-depois caixas-fechadas-antes)))
                  (t (first (estado-pontuacoes (no-estado no))))
                 ))
       (jogador2 (cond
                  ((and (= jogador 2) (< caixas-fechadas-antes caixas-fechadas-depois)) (+ (second (estado-pontuacoes (no-estado no))) (- caixas-fechadas-depois caixas-fechadas-antes)))
                  (t (second (estado-pontuacoes (no-estado no))))
                 ))
       (utilidade (- jogador2 jogador1))
      )
    (cond 
     ((equal tabuleiro-novo nil) nil)
     (t (list (list tabuleiro-novo (list jogador1 jogador2)) (1+ (no-prof no)) utilidade no))
    )
  )
)
