; R�cup�rer les premisses d'une r�gle
(defun getCond (r)
	(cadr r)
) 

; R�cup�rer la conclusion d'une r�gle 
(defun getConc (r)
	(car r)
) 

; Supprime la r�gle de la BR
(defun removeRule (r)
	; Si la r�gle existe
	(if  r
		(not (null (setq *BR* (remove r *BR*)))) ; On retourne vrai si la modificaiton � bien eu lieu (nil lorsque la BR est vide)
		nil ; Si on n'a pas trouv� la r�gle, on n'a pas pu la supprimer. Logique !
	)	
)

; Recuperer la valeur d'un �l�ment dans la BF 
(defun getValue (elem)

		(cadr (assoc elem *BF*))
	)
 

; Modifie la valeur d'un �l�ment dans la BF (s'il existe d�j�)
(defun setValue (elem value)
	(if (assoc elem *BF*)
		(setf (cadr (assoc elem *BF*)) value)
	)
)

; Permet de dire si un �l�ment est dans une liste, peu importe le type d'�l�ment
; Member ne fonctionne pas avec les cha�nes de caract�res
(defun superMember (element set)
	(let
		((found nil))
		(dolist (x set found)
			(if (equal x element)
				(setq found T)
			)
		)
	)
  )

;Print l'ensemble des symptomes trait�s par le programme
(defun printBFR()
  (format T "Liste des symptomes : ~%")
  (dolist (x *BFR*)
    (format T "~S ~%" (car x)))) 

;Forme la base de faits en interaction avec l'utilisateur
(defun createBF()
  (format T "Quel �ge a le patient ? ~%") 
 	(let ((choice (parse-integer (read-line))))
 		(if (or (not (integerp choice)) (< choice 0) (> choice 130))
 			(progn
 				(format T "~S n'est pas un choix correct. ~%" choice)
 				(return-from createBF NIL)
      )
     (add2BF (list 'age choice))))
  (format T "Quel �ge mental a le patient ? ~%") 
 	(let ((choice (parse-integer (read-line))))
 		(if (or (not (integerp choice)) (< choice 0) (> choice 130))
 			(progn
 				(format T "~S n'est pas un choix correct. ~%" choice)
 				(return-from createBF NIL)
      )
     (add2BF (list 'age_mental choice))))
  (add2BF (list 'quotient_mental (* (/ (cadr (assoc 'age_mental *BF*)) (cadr (assoc 'age *BF*))) 100)))
  (format T "Quel est le sexe du patient ? ~%") 
 	(let ((choice (read-line)))
 		(if (not (supermember choice '("homme" "femme")))
 			(progn
 				(format T "~S n'est pas un choix correct. ~%" choice)
 				(return-from createBF NIL)
 			)
     (add2BF (list 'sexe choice))))
  (format T "Quel est le poids du patient ? (en kg) ~%") 
 	(let ((choice (parse-integer (read-line))))
 		(if (or (not (integerp choice)) (< choice 0))
 			(progn
 				(format T "~S n'est pas un choix correct. ~%" choice)
 				(return-from createBF NIL)
 			)
     (add2BF (list 'poids choice))))
  (format T "Quelle est la taille du patient ? (en cm) ~%") 
 	(let ((choice (parse-integer (read-line))))
 		(if (or (not (integerp choice)) (< choice 0) (> choice 250))
 			(progn
 				(format T "~S n'est pas un choix correct. ~%" choice)
 				(return-from createBF NIL)
 			)
     (add2BF (list 'taille choice)))) 
  (add2BF (list 'imc (/ (cadr (assoc 'poids *BF*)) (expt (/ (cadr (assoc 'taille *BF*)) 100) 2))))
  (let ((answer T))
    (format T "Quels sont les symptomes ? (Entrer NIL pour quitter) ~%")
    (loop while answer do
          (printBFR)
          (setq answer (read))
          (if (assoc answer *BFR*)
              (let ((option nil))
                (format T "Pr�ciser : (entrer le num�ro correpondant) ~%" )
                (dolist (x (cadr (assoc answer *BFR*)))
                  (print x))
                (let ((option (parse-integer (read-line))))
                 
;;                (if (or (< choice 0) (> choice 10))
;;                    (progn
;; 				(format T "~S n'est pas un choix correct. ~%" choice)
;; 				(return-from createBF NIL)
;;                      )) 
                   (add2BF (list answer (cadr (assoc option (cadr (assoc answer *BFR*))))))))
            (format T "Symptome non trait� par ce programme. ~%" )))
    (format T "~%R�ccurence des symptomes ~%") 
    (dolist (x '((1 faible) (2 occasionnel) (3 regulier) (4 permanent) (5 hebdo1-3) (6 hebdo4-7) (7 hebdo8-13) (8 hebdo14+)))
                  (print x))
 	(let ((choice (parse-integer (read-line))))
 		(if (not (member choice '(1 2 3 4 5 6 7 8)))
 			(progn
 				(format T "~S n'est pas un choix correct. ~%" choice)
 				(return-from createBF NIL)
 			)
     (add2BF (list 'recurrence (cadr (assoc choice '((1 faible) (2 occasionnel) (3 regulier) (4 permanent)(5 hebdo1-3) (6 hebdo4-7) (7 hebdo8-13) (8 hebdo14+))
                                            ))))

     ))
    (format T "~%Dur�e des symptomes ~%") 
    (dolist (x '((1 >0.5) (2 >6) (3 <0.5)))
                  (print x))
 	(let ((choice (parse-integer (read-line))))
 		(if (not (member choice '(1 2 3)))
 			(progn
 				(format T "~S n'est pas un choix correct. ~%" choice)
 				(return-from createBF NIL)
 			)
     (add2BF (list 'duree (cadr (assoc choice '((1 >0.5) (2 >6) (3 <0.5))
                                            ))))

    ))
    ))
                    
              
                
;V�rifie si une pr�misse est vraie
(defun checkPremisse (p)
	(let
		(
			; On r�cup�re l'�l�ment (sous forme de liste, comme �a, si on a un OR, on r�cup�re une liste avec tous les sous-premisses)
			(element (cdr p)) 
			; L'op�rateur
			(op (car p))
			; La valeur actuelle de l'�l�ment dans la BF
			(current-value nil)
		)

		(cond

   ((equal op 'or)
        ; Si on a un or, on traite diff�rement
			(let
				((OK NIL))
     ; On v�rifie chaque sous-premisse de mani�re r�cursive
				(dolist (sous-premisse element OK)
					(setq OK (or OK (checkPremisse sous-premisse)))
				)
			)

    )
   ((equal op 'and)
        ; Si on a un and, on traite diff�rement
			(let
				((OK T))
     ; On v�rifie chaque sous-premisse de mani�re r�cursive
				(dolist (sous-premisse element OK)
					(setq OK (and OK (checkPremisse sous-premisse)))
				)
			)

    )
   ;idem pour les op�rateurs d'ordre :
   ((equal op '>)
    (if (equal (cadr p) '+)
        (> (checkPremisse (cadr p)) (caddr p))
     
   (if (getValue (cadr p))
       (> (getValue (cadr p)) (caddr p))
     NIL)
    )) 
   ((equal op '<)
    (if (equal (cadr p) '+)
        (< (checkPremisse (cadr p)) (caddr p))
      
   (if (getValue (cadr p))
       (< (getValue (cadr p)) (caddr p))
     NIL)
    )) 
   ((equal op '>=)
    (if (equal (cadr p) '+)
        (>= (checkPremisse (cadr p)) (caddr p))
      (
   (if (getValue (cadr p))
       (>= (getValue (cadr p)) (caddr p))
     NIL)
    )))
   ((equal op '<=)
    (if (equal (caadr p) '+)
        (<= (checkPremisse (cadr p)) (caddr p))
      (
   (if (getValue (cadr p))
       (<= (getValue (cadr p)) (caddr p))
     NIL)
    ))) 
   ((equal op 'if)
        ; Si on a un if, on traite diff�rement
    (if (checkPremisse (cadr p))
        1
      O)
    ) 
   ((equal op '+)
    ; Si on a un +, on traite diff�rement : ici cela servirait surtout si on d�cidait de valider une regle avec une partie seulement des premisses
    ; ex pour au moins 3 premisses sur 4 :
    ; (> (+ (if (P1) 1 0) (if (P2) 1 0) (if (P3) 1 0) (if (P4) 1 0)) 2)
    (let 
        ((SUM 0))
      (dolist (x (cdr p) SUM)
        (setq SUM (+ SUM (checkPremisse x)))
        )))
   (t ; autres cas, il y a alors 2 elements dans la premisse et il faut comparer la valeur � celle de la BF pour la m�le clef
    (if (not (supermember (car element) (cdr (assoc (car p) *BF* ))))
          (return-from checkPremisse NIL)
        T
        ) 
      ))))

; Est-ce que la r�gle est d�clenchable ?
(defun declencheable (r)
	(let
		(
			(premisses (getCond r))
			(OK T)
   )
   (setq OK (checkPremisse premisses))
   ; On v�rifie que tous les pr�misses sont vraies comme un and a �t� plac� en d�but des premisses
		)
	)


; Liste des r�gles applicables
(defun candidate-rules ()
	(let
		((candidates nil))
		(dolist (r *BR* (reverse candidates))
			; On r�cup�re la liste de toutes les r�gles d�clenchables
			(if (declencheable (symbol-value r))
				(push r candidates)
			)
		)
	)
  )



; Permet de d�clencher une r�gle 
(defun triggerRule (r)
  (let
      ; On r�cup�re le but de la fonction
      (
       (conc (getConc (symbol-value r)))
       )
    ; On v�rifie qu'elle est d�clenchable
    (if
        (declencheable (symbol-value r))
        (dolist (x conc)
          (add2BF x))) ; On ajoute le but dans la BF
    (removeRule r); On supprime la r�gle de la BR
    )
  
  (declencheable (symbol-value r)) ; On retourne T ou NIL selon si l'on a pu appliquer la r�gle
  ) 

;Ajouter un element � BF
(defun add2BF (element)
	(if (listp element)
		(if (eq (length element) 2)
      (let ((existedeja nil))
        (progn
          (dolist (x *BF* nil)
            (if (equal (car x) (car element))
                (progn
                  (setq existedeja T)
                  (let ((pres nil))
                   (progn 
                    (dolist (y (cdr x))
                      (if (equal y (cadr element))
                          (setq pres T)))
                    (if (not pres)
                        (push (cadr element) (cdr (last x)))))))))
          (if (not existedeja)
              (push element *BF*))))
			(progn
				(format T "Erreur, l'�l�ment n'est pas de la forme (nom valeur) ~%")
				(print element)
			) 
		)
		(progn
			(format T "Erreur, l'�l�ment n'est pas une liste ~%")
			(print element)
		)
	) 
)


;Moteur 1 : fonctionne dans le but de rechercher un trouble pr�cis, il s'arr�te quand il le trouve
(defun enginecible (but)
  (if *BF*
      (progn
        (let
            (
             (r (car (candidate-rules)))
             (target nil)
             (trouve nil)
             (i 0)
             )
          (format T "D�roulement du raisonnement : ~%")
          (loop while (and (not trouve) (candidate-rules)) do
                ; On r�cup�re le but avant, sinon il n'existe plus car on supprime la r�gle lors de son d�clenchement!
                (setq target (car (getConc (symbol-value r))))
                ; On d�clenche la premi�re r�gle
                (triggerRule r)
                (setq i (+ i 1))
                (format T "~% - D�clenchement de la r�gle ~S portant sur ~S ~%" r target)
                (format T "~S ~%" (car (last (symbol-value r))))
                (if (equal but (cadr target))
                    (setq trouve T))
                (setq r (car (candidate-rules))))
          (if (eq i 0)
              (format T "Vous semblez en bonne sant�. ~%" ))  
          ))))


;Moteur 2 : cherche tous les troubles probables � partir des �l�ments fournis
(defun enginegen ()
  (if *BF*
      (progn
        (let
            (
             (r (car (candidate-rules)))
             (target nil)
             (i 0)
             )
          (format T "D�roulement du raisonnement : ~%")
          (loop while (candidate-rules) do
                ; On r�cup�re le but avant, sinon il n'existe plus car on supprime la r�gle lors de son d�clenchement!
                (setq target (car (getConc (symbol-value r))))
                ; On d�clenche la premi�re r�gle
                (triggerRule r)
                (setq i (+ i 1))
                (format T "~% - D�clenchement de la r�gle ~S portant sur ~S ~%" r target)
                (format T "~S ~%" (car (last (symbol-value r))))
                (setq r (car (candidate-rules))))
          (if (eq i 0)
              (format T "Vous semblez en bonne sant�. ~%" ))  
          ))))


(defparameter R1 '(((maladie_gen trouble_neurodeveloppemntal)) (  and  (or(activite hyperactif)(activite inerte)) ( < quotient_mental 34 )) "Il est probable que vous souffrez d'un trouble neurod�veloppemental"))
(defparameter R2 '(((maladie_pre trouble_du_spectre_de_l_autisme)(maladie_pre trouble_du_developpment)(maladie_pre trouble_du_deficit_attention)) ( and (activite inerte) (< quotient_mental 34 )) "Il est probable que vous souffrez d'un trouble neurod�veloppemental, ce trouble regroupe d'autres troubles notament les troubles du d�veloppement, du spectre de l'autisme et du d�ficit de l'attention"))
(defparameter R3 '(((maladie_pre2 trouble_de_l_attention)) ( and(activite hyperactif) (crise_ment colere) (maladie_pre trouble_du_deficit_attention)) "Il est probable que vous souffrez d'un trouble de l'attention"))
(defparameter R4 '(((maladie_gen schizophrenie)) ( and (social insociable) (and(psychologie hallucination)(psychologie delire))(or (crise_physique trouble_langage)(activite  inerte))) "Il est probable que vous souffrez de schizophr�nie"))
(defparameter R5 '(((maladie_pre schizophrenie_simple)(maladie_pre schizophrenie_paranoid)(maladie_pre schizophrenie_heboidophrenie)(maladie_pre schizophrenie_hebephrenie)(maladie_pre schizophrenie_catatonique)(maladie_pre trouble_schizoaffectif)) (and (maladie_gen schizophrenie)) "Il est probable que vous souffrez de schizophr�nie, ce trouble regroupe d'autres troubles notament la schizophr�nie simple, parano�de, h�bo�dophr�nie, h�b�phr�nie, schizophr�nie catatonique, et les troubles schizoaffectifs"))
(defparameter R6 '(((maladie_pre2 schizophrenie_simple)) ( and (compassion insensible) (maladie_pre schizophrenie_simple)) "Il est probable que vous souffrez de schizophr�nie simple"))
(defparameter R7 '(((maladie_pre2 schizophrenie_paranoide)) (and (or(recurrence regulier)(recurrence permanent)) (maladie_pre schizophrenie_paranoide)) "Il est probable que vous souffrez de schizophr�nie parano�de, c'est la forme de schizophr�nie la plus courante"))
(defparameter R8 '(((maladie_pre2 schizophrenie_heboidophrenie)) (and (addiction drogue)(recurrence occasionnel)(maladie_pre schizophrenie_heboidophrenie)) "Il est probable que vous souffrez de schizophr�nie h�bo�dophr�nique"))
(defparameter R9 '(((maladie_pre2 schizophrenie_hebephrenie)) (and (recurrence faible)(stress eleve)(maladie_pre schizophrenie_hebephrenie)) "Il est probable que vous souffrez de schizophr�nie h�b�phr�nique, 20% des schizophr�nes ont cette forme de schizophr�nie"))
(defparameter R10 '(((maladie_pre2 schizophrenie_catatonique)) (and(activite inerte) (crise_physique imitation_environnement) (maladie_pre schizophrenie_catatonique)) "Il est probable que vous souffrez de schizophr�nie catatonique"))
(defparameter R11 '(((maladie_pre2 trouble_schizoaffectif)) (and(crise_physique maniaque)(duree >0.5)(crise_mental tristesse)(maladie_pre trouble_schizoaffectif)) "Il est probable que vous souffrez d'un trouble schizoaffectif"))
(defparameter R12 '(((maladie_gen bipolarite)) (and(stress eleve)(activite hyperactif)(psychologie episode_crise) (recurrence regulier)()trouble_famille oui)(and (or(crise_mental tristesse)(crise_mental depression))(crise_mental joie))(crise_physique maniaque)) "Il est probable que vous souffrez de bipolarit�")
(defparameter R13 '(((maladie_gen ep_psychotique_bref)) (and(social insociable)(and (psychologie hallucination)(psychologie delire))(and(crise_physique trouble_langage)(activite inerte))(trouble_famille oui)) "Il est probable que vous souffrez d'�pisodes psychotiques brefs"))
(defparameter R14 '(((maladie_gen boulimie)) (and(or (stress normal)(stress eleve))(nutrition sur_nutrition)(or (crise_mental inferiorite)(crise_mental culpabilte)) (or(crise_physique vomissement)(crise_physique conduite_risque))) "Il est probable que vous souffrez de boulimie"))
(defparameter R15 '(((maladie_gen kleptomanie)) (and(crise_physique vol)(recurrence regulier) (or (maladie_gen boulimie)(maladie_gen bipolarite))) "Il est probable que vous souffrez de kleptomanie"))
(defparameter R16 '(((maladie_gen PTSD)) (and(sommeil insomnie) (stress eleve) (social insociable) (crise_mental panique) (and (crise_physique tremblement)(and (crise_physique transpiration)(and (crise_physique respiration) (crise_physique coeur)))) ) "Il est probable que vous souffrez d'un trouble de stress post-traumatique"))
(defparameter R17 '(((maladie_gen somnambulisme)) (and(sommeil insomnie) (or(stress normal) (stress eleve)) (or(addiction alcool)(addiction drogue)) ) "Il est probable que vous souffrez de somnambulisme"))
(defparameter R18 '(((maladie_gen TDI)) (and(stress eleve) (psychologie amnesie) (trouble_famille oui) (and (social insociable)(social hypersociable)) (crise_mental personnalite) (and(crise_physique mutilation )(crise_physique action_obligatoire))) "Il est probable que vous souffrez du trouble dissociatif de l'identit�"))
(defparameter R19 '(((maladie_gen trouble_anxieux)) (and(duree >6 ) (recurrence regulier) (social insociable) (crise_mental panique) (addiction drogue) (and (crise_physique tremblement)(and (crise_physique transpiration)(and (crise_physique respiration) (crise_physique coeur)))) ) "Il est probable que vous souffrez d'un trouble anxieux"))
(defparameter R20 '(((maladie_pre trouble_anxieux_generalise) (maladie_pre trouble_panique) (maladie_pre trouble_phobique_specifique) ) ( and (maladie_gen trouble_anxieux)) "Il est probable que vous souffrez de trouble anxieux, ce trouble regroupe diff�rentes formes comme le trouble anxieux g�n�ralis�, le trouble panique et le trouble phobique sp�cifique"))
(defparameter R21 '(((maladie_pre2 trouble_anxieux_generalise)) (and (maladie_pre trouble_anxieux_generalise) (recurrence regulier) (duree >6) (sommeil insomnie) (heredite oui) (crise_mental colere) (crise_physique fatigue)(addiction drogue)) "Il est probable que vous souffrez de trouble anxieux g�n�ralis�"))
(defparameter R22 '(((maladie_pre2 trouble_panique)) (and(maladie_pre trouble_panique) (duree >0.5 ) (psychologie delirant) (recurrence regulier) (crise_mental panique)  (and (crise_physique tremblement)(and (crise_physique transpiration)(and (crise_physique respiration) (crise_physique coeur)))) ) "Il est probable que vous souffrez de trouble panique"))
(defparameter R23 '(((maladie_gen agoraphobie)) (and(maladie_gen trouble_anxieux) (stress eleve) (duree >6) (recurrence permanent) (social normal) (crise_mental panique) (crise_physique evite_lieu_publique) (< age 35 ) ) "Il est probable que vous souffrez d'agoraphobie"))
(defparameter R24 '(((maladie_gen trouble_humeur)) (and(or (activite hyperactif)(activite inerte)) (duree >6) (or (crise_mental tristesse)(crise_mental joie)) (crise_physique maniaque)) "Il est probable que vous souffrez de trouble de l'humeur"))
(defparameter R25 '(((maladie_gen depression)) (and(activite inerte) (recurrence permanent) (nutrition sous_nutrition) (sommeil insomnie) (social insociable) (crise_mental tristesse) ) "Il est probable que vous souffrez depression"))
(defparameter R26 '(((maladie_gen trouble_humeur)) (and(or(maladie_gen depression)(maladie_gen bipolarite))) "Il est probable que vous souffrez de trouble de l'humeur"))
(defparameter R27 '(((maladie_gen dysmorphophobie)) (and(or(stress normal)(stress eleve)) (or (recurrence regulier)(recurrence permanent)) (crise_mental inferiorite) (crise_physique soucis_apparence) (> age 10 ) ) "Il est probable que vous souffrez de dysmorphophobie"))
(defparameter R28 '(((maladie_gen trichotillomanie)) (and(or(stress normal)(stress eleve)) (or(recurrence regulier)(recurrence permanent)) (crise_physique  arrache_cheveux)) "Il est probable que vous souffrez de trichotillomanie"))
(defparameter R29 '(((maladie_gen obsession)) (and(or(stress normal)(stress eleve)) (or (recurrence regulier) (recurrence permanent)) (crise_mental action_inquietude) (psychologie deni_symptome) ) "Il est probable que vous souffrez d'obsessions"))
(defparameter R30 '(((maladie_gen compulsion)) (and(or(stress normal)(stress eleve)) (or (recurrence regulier) (recurrence permanent)) (crise_physique action_obligatoire) (psychologie sentiment_accomplissement)) "Il est probable que vous souffrez de compulsions"))
(defparameter R31 '(((maladie_pre trouble_obsessionnel_compulsif)) (and(or(maladie_gen obsession)(maladie_gen compulsion)) ) "Il est probable que vous souffrez de trouble obsessionnel compulsif"))
(defparameter R32 '(((maladie_gen anorexie_nerveuse)) (and(crise_physique menstruation_aucune)(sexe femme)(< IMC 18.4 )) "Il est probable que vous souffrez d'anorexie nerveuse"))
(defparameter R33 '(((maladie_gen anorexie_nerveuse)) (and(poids constant)(< age 8 )) "Il est probable que vous souffrez d'anorexie nerveuse"))
(defparameter R34 '(((maladie_gen anorexie_nerveuse)) (and(recurrence regulier)(< IMC 18.4 )) "Il est probable que vous souffrez d'anorexie nerveuse"))
(defparameter R35 '(((maladie_pre anorexie_nerveuse_grade_I)) (and(maladie_gen anorexie_nerveuse)(and (< IMC 18.4 )(> IMC 17 ))) "Il est probable que vous souffrez d'anorexie nerveuse de grade I"))
(defparameter R36 '(((maladie_pre anorexie_nerveuse_grade_II)) (and(maladie_gen anorexie_nerveuse)(and (< IMC 17 )(> IMC 15 ))) "Il est probable que vous souffrez d'anorexie nerveuse de grade II"))
(defparameter R37 '(((maladie_pre anorexie_nerveuse_grade_III)) (and(maladie_gen anorexie_nerveuse)(and (< IMC 15 )(> IMC 13 ))) "Il est probable que vous souffrez d'anorexie nerveuse de grade III"))
(defparameter R38 '(((maladie_pre anorexie_nerveuse_grade_IV)) (and(maladie_gen anorexie_nerveuse)(and (< IMC 10 )(> IMC 13 ))) "Il est probable que vous souffrez d'anorexie nerveuse de grade IV"))
(defparameter R39 '(((maladie_pre anorexie_nerveuse_grade_V)) (and(maladie_gen anorexie_nerveuse)(< IMC 10 )) "Il est probable que vous souffrez d'anorexie nerveuse de grade V"))
(defparameter R40 '(((crise_physique vomissement)) (and(crise_physique erosion_dent)) "Vous avez des vomissements"))
(defparameter R41 '(((crise_physique conduite_risque)) (and(or (or (crise_physique tentative_suicide) (crise_physique mutilation))(or(addiction drogue) (addiction alcool)))) "Vous avez une conduite � risque"))
(defparameter R42 '(((maladie_pre boulimie_nerveuse)) (and(maladie_gen boulimie) (< IMC 25 )) "Il est probable que vous souffrez de boulimie nerveuse"))
(defparameter R43 '(((maladie_pre hyperphagie_boulimique)) (and(maladie_gen boulimie) (> IMC 25 )) "Il est probable que vous souffrez de hyperphagie boulimique"))
(defparameter R44 '(((maladie_pre2 hyperphagie_boulimique_legere)) (and(maladie_pre hyperphagie_boulimique) (and(> recurrence 1 )(< recurrence 3 ))) "Il est probable que vous souffrez de hyperphagie boulimique l�g�re"))
(defparameter R45 '(((maladie_pre2 hyperphagie_boulimique_moderee)) (and(maladie_pre hyperphagie_boulimique) (and(> recurrence 4 )(< recurrence 7 ))) "Il est probable que vous souffrez de hyperphagie boulimique mod�r�e"))
(defparameter R46 '(((maladie_pre2 hyperphagie_boulimique_severe)) (and(maladie_pre hyperphagie_boulimique) (and( > recurrence 8 )(< recurrence 13 ))) "Il est probable que vous souffrez de hyperphagie boulimique s�v�re"))
(defparameter R47 '(((maladie_pre2 hyperphagie_boulimique_extreme)) (and(maladie_pre hyperphagie_boulimique) (> recurrence 14 )) "Il est probable que vous souffrez de hyperphagie boulimique extr�me"))
(defparameter R48 '(((maladie_gen q_i_normal)) (and (> quotient_mental 80)(< quotient_mental 119) ) "Vous semblez avoir un quotient intellectuel normale"))
(defparameter R49 '(((maladie_gen retard_ment_grave))(and (> quotient_mental 20)(< quotient_mental 40) ) "Vous souffrez probablement d'un retard mental grave"))
(defparameter R50 '(((maladie_gen retard_ment_l_m))(and (> quotient_mental 40)(< quotient_mental 70) ) "Vous souffrez probablement d'un retard mental moyen � l�ger"))
(defparameter R51 '(((maladie_gen retard_ment_prof))((< quotient_mental 20) ) "Vous souffrez probablement d'un retard mental profond"))
(defparameter R52 '(((nutrition sur_nutrition))((> IMC 30) ) "Votre IMC est tr�s haut, vous semblez �tre en sur-nutrition"))
(defparameter R53 '(((nutrition sous_nutrition))((< IMC 18) ) "Votre IMC est tr�s bas, vous semblez �tre en sous-nutrition"))
(defparameter *BR* '(R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15 R16 R17 R8 R19 R20 R21 R22 R23 R24 R25 R26 R27 R28 R29 R30 R31 R32 R33 R34 R35 R36 R37 R38 R39 R40 R41 R42 R43 R44 R45 R46 R47))

(defparameter *BFR* '(
                      (nature_symptome ( (1 stress ) (2 activite) (3 psychologie) (4 nutrition) (5 compassion) (6 trouble_famille) (7 sommeil) (8 social) (9 crise_mental) (10 crise_physique) (11 addiction)))
                      (stress ((1 faible) (2 normal)  (3 eleve)))
                      (activite ( (1 hyperactif) (2 inerte) (3 normal)))
                      (psychologie ( (1 stable) (2 episode_crise) (3 delire) (4 hallucination) (5 amnesie) (6 deni_symptome) (7 sentiment_accomplissement)) )
                      (nutrition ((1 sous_nutrition)(2 normal)(3 sur_nutrition)))
                      (heredite ( (1 oui) (2 non)))
                      (compassion ((1 insensible)(2 normal) (3 hypersensible)))
                      (trouble_famille ( (1 oui) (2 non)))
                      (sommeil ( (1 insomnie) (2 normal) (3 hypersomnie)))
                      (social ((1 insociable) (2 normal) (3 hyper_sociable)))
                      (crise_mental ( (1 personnalite) (2 superiorite) (3 inferiorite) (4 tristesse) (5 existentielle) (6 joie) (7 panique) (8 colere) (9 culpabilite)))
                      (crise_physique ((1 respiration) (2 coeur) (3 transpiration) (4 pleure) (5 tremblement) (6 arrache_cheveux) (7 mutilation) (8 action_obligation) (9 maniaque) (10 trouble_langage) (11 imitation_environnement) (12 vomissements) (13 fatigue) (14 vertige) (15 douleur_thoracique) (16 evite_lieu_publique) (17 soucis_apparence) (18 vol) (19 tentative_suicide) (20 menstruation _absentes) ( 21 erosion_dent) (22 conduite_risque)))
                      (addiction ( (1 alcool) (2 drogue) (3 aucune)))
                      ))

(defparameter *BF* '())

(defun menuSE()
  (defparameter *BR* '(R1 R2 R3 R4 R5 R6 R7 R8 R9 R10 R11 R12 R13 R14 R15 R16 R17 R8 R19 R20 R21 R22 R23 R24 R25 R26 R27 R28 R29 R30 R31 R32 R33 R34 R35 R36 R37 R38 R39 R40 R41 R42 R43 R44 R45 R46 R47))


              (format T "~%")
              (format T "_________________________________________________~%")
              (format T "|                     Menu                      |~%")
              (format T "_________________________________________________~%")
              (format T "| Que souhaitez-vous faire ?                    |~%")
              (format T "| 1 - Entrer profil patient                     |~%")
              (format T "| 2 - Lancer recherche pr�cise                  |~%")
              (format T "| 3 - Lancer recherche g�n�rale                 |~%")
              (format T "| 4 - Patients tests pr�enregistr�s             |~%")
              (format T "_________________________________________________~%")
              (format T "~%") ; Petit saut de ligne au calme pour le beau jeu     
              (let
                  ((choice (read-line)))
                (setq choice (parse-integer choice))
                (cond
                 ((or (< choice 1) (> choice 4))
                  (format T "Ce choix n'est pas valide. ~%")
                  )
                 ; Cr�ation profil patient
                 ((eq choice 1)
                  (progn
                    (createBF)
                    (menuse))
                  )
                 ; Choix du dossier
                 ((eq choice 2)
                  (progn
                    (format T "Entrer le nom du trouble recherch� : ~%")
                    (dolist (x *br*)
                      (print (cadr (car (getconc (symbol-value x))))))
                    (let
                        ((cible (read-line)))
                      (enginecible cible))
                    (defparameter *BF* '())))
                 ((eq choice 3)
                  (progn
                    (enginegen)
                    (defparameter *BF* '())
                    ))
                 ((eq choice 4)
                  (menupatientest))
                  )))
                 
(defun menupatientest()
  (format T "~%")
  (format T "__________________________________________________~%")
  (format T "|          Menu patients pr�enregistr�s           |~%")
  (format T "__________________________________________________~%")
  (format T "| Lequel choississez-vous ?                       |~%")
  (format T "| 1 - Patient 1 (D�pression, schizophr�nie)       |~%")
  (format T "| 2 - Patient 2 (Conduite � risque)               |~%")
  (format T "| 3 - Patient 3 (sain)                            |~%")
  (format T "__________________________________________________~%")
  (format T "~%") ; Petit saut de ligne au calme pour le beau jeu
  (let
                  ((choice (read-line)))
                (setq choice (parse-integer choice))
                (cond
                 ((or (< choice 1) (> choice 3))
                  (format T "Ce choix n'est pas valide. ~%")
                  )
                 ; Cr�ation profil patient
                 ((eq choice 1)
                  (progn
                    (defparameter *BF* '((sexe homme) (age 30) (age_mental 30) (poids 75) (taille 189) (social insociable) (psychologie hallucination delire) (activite inerte) (recurrence permanent) (nutrition sous_nutrition) (sommeil insomnie) (crise_mental tristesse) ))
                    (dolist (x *bf*)
                      (print x))
                    (enginegen)
                    (defparameter *BF* '())
                    )
                  )
                 ; Choix du dossier
                 ((eq choice 2)
                  (progn
                    (defparameter *BF* '((sexe femme) (age 14) (age_mental 15) (poids 30) (taille 155) (social normal) (psychologie hallucination delire) (activite inerte) (recurrence permanent) (addiction oui) (crise_physique tentative_suicide) ) )
                    (dolist (x *bf*)
                      (print x))
                    (enginegen)
                    (defparameter *BF* '())
                    )
                  )
                 ((eq choice 3)
                  (progn
                    (defparameter *BF* '((sexe femme) (age 20) (age_mental 20) (poids 55) (taille 155) (IMC 22.9) (social normal)))
                    (dolist (x *bf*)
                      (print x))
                    (enginegen)
                    (defparameter *BF* '())
                    )
                  ))))

(defun main ()
  (menuse))

(main)

  