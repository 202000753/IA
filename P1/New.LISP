;;;; new.lisp
;;;; Novas funções a incluir nos ficheiros
;;;; Autores: Nuno Reis, Vitor Nunes

(defvar *jogador* 1)
(defvar *computador* 2)

(defun operadores ()
  (list 'arco-horizontal 'arco-vertical)
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

;;;;;;;
;MinMax - Cortes AlfaBeta
;;;;;;;
(defun jogada-computador (tabuleiro)
  (alfabeta (list (list tabuleiro (list 4 8)) 0 4 nil))
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

(defun alfabeta (no &optional (alfa (- 0 999)) (beta 999))
  (let*
      (
       (tabuleiro (estado-tabuleiro (no-estado no)))
       (tipo-jogador (cond ((equal (evenp (no-prof no)) t) *computador*) (t *jogador*)))
       (sucessores-no (sort (remove nil(sucessores-alfabeta no tipo-jogador)) #'> :key #'no-utilidade))
      )
    (cond
     ((equal (tabuleiro-preenchidop tabuleiro) t) (no-utilidade no))
     ((= tipo-jogador *computador*) (alfabeta-max no alfa beta sucessores-no))
     ((= tipo-jogador *jogador*) (alfabeta-min no alfa beta sucessores-no))
    )
  )
)

(defun alfabeta-max (no alfa beta sucessores-no)
  (let
      (
       (new-alfa (let
                     (
                      (aa (alfabeta (car sucessores-no) alfa beta))
                     )
                   (cond
                    ((< alfa aa) aa)
                    (t alfa)
                   )))
      )
    (cond
     ((> new-alfa beta) beta)
     ((equal (cdr sucessores-no) nil) new-alfa)
     (t (alfabeta-max no new-alfa beta (cdr sucessores-no)))
    )
   )
)

(defun alfabeta-min (no alfa beta sucessores-no)
  (let
      (
       (new-beta (let
                     (
                      (aa (alfabeta (car sucessores-no) alfa beta))
                     )
                   (cond
                    ((> beta aa) aa)
                    (t beta)
                   )))
      )
    (cond
     ((> alfa new-beta) alfa)
     ((equal (cdr sucessores-no) nil) new-beta)
     (t (alfabeta-max no alfa new-beta (cdr sucessores-no)))
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


