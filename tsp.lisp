; from initial state - 2d array w distances between cities.

; states are represented by city ID , cost, visited vector

;; operator. 

;; receives state (takes ID and visited vector from there)



; 1st check which cities already on visited vector 


;; for the ones who're not

;; consults graph (external to a* problem representation) -> to get distances from current city (row on graph matrix)



;;return  list w all new cities (id, list of visited cities on this state, distance from previous city)


;; a star procedure

;; at each step.

;; g func takes state -> gets last value from list (distance)

; h func takes state and does min span tree: 

;; num of cities is length of vector numOfCities (initially w all 0, than 1)

;; calls msp-h with the following  edges (from this procedure):
;; ATENTION - Test java version and see diferences. if there are, probably because we need to take number of cities (nodes) into account when doing algorithm itself
;; for(int i = 0; i < numOfCities; i++){								//TC: we are taking upper triangular matrix
	;;		for(int j = i + 1; j < numOfCities; j++){
			;;	if(visited[i] == 0 && visited[j] == 0){
			;;		spanTreeEdge tempEdge = new spanTreeEdge(i, j, DISTANCE in graph);

(defun heuristica (state)
(let ( (edges (list) ) )
			(progn 

				(dotimes (i (list-length (state-visited state) ))

					(progn 
					;put all edges from current city to depot ...
													(if (and (equalp (nth i (state-visited state)) 0)  (not (equalp (aref test-graph i 0) -1))) (setf edges (append edges (list (list i 0 (aref test-graph i 0))) )) )
													(loop for j from (1+ i) to (list-length (state-visited state) ) 
			do  
				


						(if (and (equalp (nth i (state-visited state)) 0)  (equalp (nth j (state-visited state)) 0)

			 (not (equalp (aref test-graph i j) -1))) 

				(setf edges (append edges (list (list i j (aref test-graph i j))) )) 

				) 
					

			)
						)

		
		)
(return-from heuristica  (mst-h edges))


				)

				

				)

	
	)


(defun heuristica-old (state)
(let ( (edges (list) ) )
			(progn 

				(dotimes (i (list-length (state-visited state) ))
		(loop for j from (1+ i) to (list-length (state-visited state) ) 
			do  


			(if (and (equalp (nth i (state-visited state)) 0)  (equalp (nth j (state-visited state)) 0)

			 (not (equalp (aref test-graph i j) -1))) 

				(setf edges (append edges (list (list i j (aref test-graph i j))) )) 

				) )
		)
(return-from heuristica-old (mst-h edges))


				)

				

				)

	
	)


;; a star - termination condition? Agent has visited all cities and its on the initial city. state has (0, [1...k], dist from last city to depot)
(defstruct state
  cityID
  visited
  distance)

; initial state is already visited. we need to change termination condition for algorithm to get to this point (eventhough its 1)
(setf initial-state (make-state :cityID 0 :visited '(1 0 0 0 0 0 0 0) :distance 0))

(setf random-state (make-state :cityID 2 :visited '(1 0 1 0 0 0 0 0) :distance 35))
(setf final-state (make-state :cityID 0 :visited '(1 1 1 1 1 1 1 1) :distance 0))

(setf subfinal-state (make-state :cityID 5 :visited '(1 1 1 1 1 1 1 1) :distance 35))


 (setf test-graph (make-array '(8 8) :initial-contents '((00 03 -1 -1 -1 02 06 -1) (03 00 04 05 -1 05 -1 -1) (-1 04 00 -1 05 -1 -1 02) (-1 05 -1 00 04 -1 04 -1)  (-1 -1 05 04 00 -1 -1 04) (02 05 -1 -1 -1 00 03 -1) (06 -1 -1 04 -1 03 00 02) (-1 -1 02 -1 04 -1 02 00)   )))

;; DOMAIN - City IDS from 0 to num-row. visited vector as input of first state must have num-rows size
;; first check row in graph for city id
;; then see which cities are already in visited vector (Value = 1)
;; for the one's who're not generate new cities w visited vector with 1 on the city id visited, and distance from current city
(defun operator (state) 
(if (not (member 0 (state-visited state)))
					;; if we're on the last city we need to check if there's a path to the depot
					(if (equalp (aref test-graph (state-cityID state) 0) -1) 
							(return-from operator (list))
											(return-from operator (list (make-state :cityID 0 :visited (make-list (list-length (state-visited state)) :initial-element 1) :distance (aref test-graph (state-cityID state) 0))))
						)


					(let ((states (list)) (temp-visited nil)) 
		(progn  
				(dotimes (i (array-dimension test-graph 1))
         
          (if (equalp (nth i (state-visited state)) 0)
          	(if (not (equalp (aref test-graph (state-cityID state) i) -1)) 

          			(progn (setf temp-visited (copy-list (state-visited state)))
          		(setf (nth i temp-visited) 1)
          	 (setf states (append states (list (make-state :cityID i :visited temp-visited :distance (aref test-graph (state-cityID state) i)) ))) 
          	 )
          		)


          	

             ) )
			(return-from operator states))

			 
		)


					)


	)

(defun objetivo (state)
	(and (equalp (state-cityID state) 0) (not (member 0 (state-visited state)))  (not (equalp (state-distance state) -1)) ) )

(defun custo (state) 
	(state-distance state))

(setf problema (cria-problema initial-state (list 'operator) :objectivo? #'objetivo :heuristica 'heuristica :custo 'custo))

(defun result (problema) 
(let ((path (list))) 
	(progn 
			(dolist (state (first (procura problema "a*" :espaco-em-arvore? t)))
	 	(setf path (append path (list (state-cityID state)) ))
	  )
			(return-from result path)
		)
	 

	))
