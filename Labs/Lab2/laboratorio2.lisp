;;;; laboratorio2.lisp
;;;; Disciplina de IA - 2022 / 2023
;;;; Ficha de Laboratório nº2
;;;; Autor: Nuno Reis

;;; Para Debug
;; soma-3
(defun soma-3 (a b c)
"Função que faz a soma de 3 números passados como argumento"
	(+ a b c)
)

;;; Exercicios sobre funções em Lisp (não recursivas)
;; notas-dos-alunos
(defun notas-dos-alunos ()
"Função que retorna 3 listas de 4 notas"
	'((15.5 15 8.25 13) (17.5 11 9 13.25) (11.75 0 0 16))
)

;; notas-do-primeiro-aluno
(defun notas-do-primeiro-aluno (a)
"Função que retorna a lista das 4 notas do primeiro aluno da lista"
	(car a)
)

;; calcula-media-notas
(defun calcula-media-notas (a)
"Função que recebe uma lista de 4 valores e retorna a média"
	(/ (+ (first a) (second a) (third a) (fourth a)) 4)
)

;; maior-nota-do-aluno
(defun maior-nota-do-aluno (a)
"Função que devolve a maior nota de um conjunto de 4 notas"
	(max (first a) (second a) (third a) (fourth a))
)