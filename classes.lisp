;;; Assuming that all inputs are syntactically correct.

;;maybe I should create a domain/range sent (declarationsent)

;;consertar as classes

(defconstant *logsymbols* '(AND OR NOT => <=>))
(defconstant *quantsymbols* '(FORALL EXISTS))

(defclass quantsent ()
  ((variables :accessor variables)
   (sentence :accessor sentence)))

(defclass equation ()
  ((term-1 :accessor term-1)
   (term-2 :accessor term-2)))

(defclass logsent ()
  ((logoperator :accessor logoperator)
   (sentences :accessor sentences)))
;;No caso do or e do and, que são variáveis no número de argumentos; o que fazer?
;;talvez binarizar seja adequado.

(defclass relsent ()
  ((relword :accessor relword)
   (args :accessor args)))

(defclass funterm ()
  ((funword :accessor funword)
   (args :accessor args)
   (return-type :accessor return-type)))

;;classe das constantes: (?) [type é o instance do que ela é, colocar subclass]
(defclass word ()
  ((name :accessor name)
   (type :accessor type)))

;;nas variáveis, elas vão precisar assumir tipos específicos dependendo da
;;sentença; além de que talvez seja relevante saber se é rowvar ou objvar.
;;ver se essa coisa de saber se é row é util ou não
(defclass variable ()
  ((name :accessor name)
   (type :accessor type)
   (row? :accessor row?)))

;;fparse retorna a formula f-in parseada.
;;q é uma fila (ainda não adaptei o código pros casos de indeterminação)
;;c é o contexto da transformação.
;;Ele diz se o termo é função, relação ou uma constante.

;; A VER: c pode ser uma hash table; se a palavra for levada num par, é função
;;(no caso, o par seria uma lista com o tipo dos argumentos, domain;
;;e o segundo elemento é o tipo do retorno, range), se num par com
;;segundo elemento vazio, é relação (relação não tem range); se for levada num
;;tipo é uma constante (ou tratar como função de argumento 0, a ver).

;;Além disso, p/ descobrir que uma palavra é uma relword, não basta ver se é
;;instância de relation; é preciso armazenar todas

(defun check-function (c word) 
  ;;checa se a palavra é uma funword num dado contexto c que é bem formado
  ;;de acordo com as regras do programa (ainda não definido).
  )

(defun check-relation (c word)
  ;;checa se a palavra é uma funword num dado contexto c que é bem formado
  ;;de acordo com as regras do programa (ainda não definido).

  )

(defun refresh-context (c sent)
  ;;checa se precisa mexer no contexto e o faz se necessário.
  
  )

(defun fparse (c f-in f-out q)
  (if (listp f-in)
      (let ((head (car f-in)))
	(cond
	  ((member head *quantsymbols*)  ;;QUANTSENT
	   (let (sent (make-instance 'quantsent))
	     (setf (variables sent) (cadr f-in)) ;;parsear as variáveis (*)
	     (setf (sentence sent) (fparse c (cddr f-in) f-out q)) ;;?
	     sent))
	  ((equal head '=)
	   (let (sent (make-instance 'equation))
	     (setf (term-1 sent) (fparse c (cadr f-in) f-out q))
	     (setf (term-2 sent) (fparse c (caddr f-in) f-out q))
	     sent))
	  ((member head *logsymbols*)    ;;LOGSENT
	   (let (sent (make-instance 'logsent))
	     (mapcar #'(lambda (sent-in)
			 (funcall #'fparse c sent-in f-out q))
		     (cdr sent)))) ;;conferir se isso está certo
	  ;;daqui, ou é um relsent ou um funterm
	  ((check-function c head)
	   (let (sent (make-instance 'funterm))
	     (setf (funword sent) head)
	     (setf (args sent)  ) ;; ****** aqui faz algo parecido com a lista de variáveis em quantsent.

	     sent))
	  ((check-relation c head)
	   (let (sent (make-instance 'relsent))
	     (setf (relword sent) head)
	     (setf (args sent)  ) ;; **********de novo
	     (refresh-context c f-in)
	     sent
	     ))
	  (t)  ;;variável posiçao de relword (?)
	  ))
      (if (or (equal (char (string f-in) 0) #\?)
	      (equal (char (string f-in) 0) #\@)) ;;rever 
	  (let (sent (make-instance 'variable))
	    ;;tipo(?)
	    (setf (name sent) f-in)
	    (setf (row? sent) (equal (char (string f-in) 0) #\@))
	    sent)
	  (let (sent (make-instance 'word))
	    (setf (name sent) f-in)))))

;;;(*) Esse caso precisa ser visto a parte, visto que vai ser uma lista de
;;;variáveis, o que não é previsto pro fparse fazer. E.g.:
;;;(forall (?X ?Y ?Z) (...)); precisa parsear (?X ?Y ?Z) como 3 variáveis;
;;;ele vai tentar ler como relação ou função (ou não; talvez ele não avalie yz)
