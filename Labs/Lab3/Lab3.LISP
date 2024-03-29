;Exemplos
(defun entre-intervalo-cond (x)
"Fun��o que verifica se um numero esta entre 10 e 20 (usa o cond)"
 (cond
  ((and (numberp x) (> x 10) (< x 20)) (format t "~d � maior que 10 e menor que 20" x))
  (t (format t "n�o � um n�mero entre 10 e 20"))
 )
)

(defun entre-intervalo-if (x)
"Fun��o que verifica se um numero esta entre 10 e 20 (usa o if)"
 (if (and (numberp x) (> x 10) (< x 20))
  (format t "~d � maior que 10 e menor que 20" x) ; verdadeiro
  (format t "~d n�o � um n�mero entre 10 e 20" x) ; falso
 )
)

;1
(defun entre-intervalo (n lista)
"Fun��o que verifica se um numero esta entre dois numeros (usa o cond)"
 (cond
  ((and (numberp n) (> n (first lista)) (< n (second lista))) (format t "~d � maior que ~d e menor que ~d" n (first lista) (second lista)))
  (t (format t "n�o � um n�mero entre ~d e ~d" (first lista) (second lista)))
 )
)

;2
(defun max-3 (a b c)
"Fun��o que retorna o maior valor entre 3 numeros"
 (cond
  ((and (> a b) (> a c))(format t "~d" a))
  ((and (> b a) (> b c))(format t "~d" b))
  ((and (> c a) (> c b))(format t "~d" c))
 )
)

;3
(defun restop (do dr r)
"Fun��o que verifica se o resto da divis�o enter os dois primeiros numeros � igual ao terceiro"
 (cond
  ((= (mod do dr) r) t)
  (t nil)
 )
)

;4
(defun calcula-media-notas (a)
"Fun��o que recebe uma lista de 4 valores e retorna a m�dia"
 (/ (+ (first a) (second a) (third a) (fourth a)) 4)
)

(defun aprovadop (lista)
"Fun��o que verifica se o aluno est� aprovado (se a primeira e ultima nota forem maiores que 9,5 ou se a media das 4 notas for 9,5)"
 (cond
  ((and (>= (first lista) 9.5) (>= (fourth lista) 9.5)) t)
  ((>= (calcula-media-notas lista) 9.5)  t)
  (t nil)
 )
)

;5
(defun nota-valida (nota)
"Fun��o que verifica se uma nota � valida (entre 0 e 20)"
 (cond
  ((and (>= nota 0) (<= nota 20)) t)
  (t nil)
 )
)

(defun poderacoes-validas (ponderacoes)
"Fun��o que verifica se as pondera��es estam corretas (a soma � 100))"
 (cond
  ((= (+ (first ponderacoes) (second ponderacoes) (third ponderacoes)) 100) t)
  (t nil)
 )
)

(defun nota-final (notas  ponderacoes)
"Fun��o que verifica se o aluno est� aprovado (se a primeira e ultima nota forem maiores que 9,5 ou se a media das 4 notas for 9,5)"
 (cond
  ((and (nota-valida (first notas)) (nota-valida (second notas)) (nota-valida (third notas)) (poderacoes-validas ponderacoes)) (+ (* (first notas) (/ (first ponderacoes) 100)) (* (second notas) (/ (second ponderacoes) 100)) (* (third notas) (/ (third ponderacoes) 100))))
  (t nil)
 )
)

;6
(defun produto-somas (lista1  lista2)
"Fun��o que adiciona os membros de duas lista e devolve o produto"
 (* (+ (first lista1) (first lista2)) (+ (second lista1)(second lista2)) (+ (third lista1) (third lista2)))
)

;7
(defun junta-listas-tamanho-igual (lista1  lista2)
"Fun��o que junta duas listas do mesmo tamanho"
 (cond
  ((= (list-length lista1) (list-length lista2)) (append lista1 lista2))
  ((> (list-length lista1) (list-length lista2)) lista1)
  ((< (list-length lista1) (list-length lista2)) lista2)
 )
)

;8
(defun dois-ultimos-elementos (lista)
"Fun��o que retorna os dois ultimos elementos de uma lista"
 (cond
  ((null lista) nil)   
  (t (cons (nth 0 (reverse lista)) (nth 1 (reverse lista))))
 )
)

;9
(defun palindromop (lista)
  (cond
   ((equal lista (reverse lista)) t)
   (t nil)
  )
)

;10
(defun criar-pares (lista1 lista2)
  (cond 
   ((or (/= (list-length lista1) (list-length lista2)) (null lista1) (null lista2)) nil)
   (t (cons (cons (first lista1) (first lista2)) (cons (cons (second lista1) (second lista2)) (cons (third lista1) (third lista2)))))
  )
)

;11
(defun verifica-pares (lista)
  (append () (cond
               ((restop (first lista) 2 0) '(t))
                (t '(nil))
               ) (cond
                  ((restop (second lista) 2 0) '(t))
                  (t '(nil))
                 ) (cond
                     ((restop (third lista) 2 0) '(t))
                     (t '(nil))
                    )(cond
                      ((restop (fourth lista) 2 0) '(t))
                      (t '(nil))
                     ))
)

;12
(defun rodar (lista sentido)
  (cond
   ((equal sentido 'esq) (cons (cons (fourth lista) (first lista)) (cons (second lista) (third lista))))
   ((equal sentido 'dir) (cons (cons (second lista) (third lista)) (cons (fourth lista) (first lista))))
   (t nil)
  )
)

;13