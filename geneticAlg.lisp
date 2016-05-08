;;How the code works
;;The code get a parameter list
;;Then he generate a population of the size
;;And get the player power
;;That way,he generate many combinations that match with the player power or get closer
;;So he can automaticly generate enemies
;;In this showcase , the code use armor material
;;But the code can be adapted to diferent tipe of enemies
;;It's also can be used together with bone based animations to make many types of enemies combinations



;;Some parameters
;;Here few parameters are in portuguese
;;Here whe generate enemys with random armors materials
(defparameter cont 0)

;;In the parameter the first element of the list is the name
;;The second is the power of that element 
(defparameter arms '(
		     (steel 20)
		     (iron 35 )
		     (wood 5)
		     (meat 10)
		     (diamond 50)
		     (vibranium 200)
		     (Adamantium 100)
		     (cloth 15)
		     (nothing 0)
		     ))
;;torço means body in portuguese
(defparameter torco '((steel 50)
		      (iron 70)
		      (cloth 15)
		      (wood 20)
		      (meat 30)
		      (diamond 150)
		      (Adamantium 300)
		      (vibranium 600)
		      (potato 0) 
		      ))
;;arma means weapon in portuguese
(defparameter arma '((blaster 100)
		     (shotgun 50)
		     (pistol 20)
		     (sword 35)
		     (Axe 40)
		     (Banana 1)
		     (Dinamite 70)
		     (rifle 80)
		     (light-saber 300)
		     (nothing 0 )))
;;pernas means legs in portuguese
(defparameter pernas '((steel 15)
		       (iron 30)
		       (wood 5)
		       (cloth 15)
		       (meat 10)
		       (diamond 50)
		       (Adamantium 100)
		       (vibranium 200)
		       (sem 0 )))
(defparameter popul '())
(defparameter child '())
(defparameter awsn '())
;;--------------------------------------------------------------------
;;This function generate the person

(defun genPerson (power)
  (let ((lista  (append (nth (random (length arms)) arms) (nth (random (length arma)) arma) (nth (random (length pernas )) pernas) (nth (random (length torco) ) torco))))
    (cons (fitness lista power) lista)))

;;-------------------------------------------------------------------
;;Calculate the fitness
;;Lower the fitness,better the person is 
(defun fitness (lista power)
  (abs(- power (+ (nth 1 lista) (nth 3 lista) (nth 5 lista) (nth 7 lista))))
  )

;;----------------------------------------------------------------
;;This function is responsable for creating the population


(defun genPopul (size power)
  (loop for i below size
     do (push (genPerson power) popul)
       )
  popul)
;;------------------------------------------------------------------
;;This function get the car of each list
;;and order the list using the < predicate
;;That way,who has the lower fitness(car) get in the top

(defun sortByFitness ()
  (sort popul '< :key #'car))

;;This function show the power
;;That return the full power of the person
(defun poder (lista)
  (+ (third lista) (fifth lista) (nth 6 lista) (car (last lista))))
;;-------------------------------------------------------------------
;;A function to print
(defun showing (lista)
  (format nil "Total power:~A~%Arms:~A || Weapon:~A || Legs:~A || Body:~A ~%in ~a repetitions"(poder lista) (second lista) (nth 3 lista) (nth 5 lista) (nth 7 lista) cont))
;;--------------------------------------------------------------------		
;;This function make the crossover of the 2 best fitness of the population
;;Afther this , we keep the parents and the 2 new sons in the population
;;and generate a new popul with length -4
(defun crossOver (lista1 lista2 power)
  (setq child '())
  (let ((child1 '())
	(child2 '()))
    (setq child1 (append (list (nth 1 lista1) (nth 2 lista1) (nth 3 lista1) (nth 4 lista1)) (nthcdr 5 lista2)))
    (setq child2 (append (list (nth 1 lista2) (nth 2 lista2) (nth 3 lista2) (nth 4 lista2)) (nthcdr 5 lista1)))
    (setq child1 (cons (fitness child1 power) child1))
    (setq child2 (cons (fitness child2 power) child2))
    (push child1 child)
    (push child2 child)
    (push lista1 child)
    (push lista2 child)))


;;SavingResult
;;This function save in a file called "popul.txt"
;;You can send another file name
;;(savingResult)
;;This function can be removed from the main function if you want to improve your speed and dove save the result

(defun savingResult ()
  (let ((in (open  "popul.txt" :if-does-not-exist :create :if-exists :overwrite )))
    (print popul)))

;;Size is the size of population
;;Power is the power of your character
;;Ex: power = heroLevel * difficulty
;;So the algorithm will search for a enemy with familiar power to generate
;;(main 1000000 300)
;;Create a 1.000.000 population and search for a enemy with 300 power 
;;"Total power:300
;;Arms:STEEL || Weapon:RIFLE || Legs:VIBRANIUM || Body:POTATO 
;;in 2 repetitions"
;;With 1.000.000 population usually take 4seconds to run
;;If you run with a lower population it get the result faster
;;But may not get the best result
;;If you put a power that is impossible to reach
;;He will run 1000 times before stop
;;After stop will return the best fit for the power
(defun main (size  power ) 
  (setq popul '())
  (genPopul size power)
  (sortByFitness)
  (setf cont 0 )
  (loop for i below 1000
     do
       (cond ((= (caar popul) 0) (return cont))
	     (t 
	      (setf cont (1+ cont))
	      ;;The next setf can cause lost of speed
	      ;;But he is essential
	      ;;Whenever you use sort,if you dont take care you lose data
	      ;;This setf is responsable for not loosing data
	      (setf popul(sortByFitness)) 
	      (crossover (first popul) (second popul) power)
	      (setq popul '())
	      (genPopul (- size 4 ) power)
	      (setq popul (append child popul))
	      )))
  ;;If you want to save your population in a file you can use the following line
  ;;(savingResult)
  ;;You can make showing show more persons with 0 fitness (0 fitness means the power of the enemy is equal of the hero)
  (showing (car popul)))

