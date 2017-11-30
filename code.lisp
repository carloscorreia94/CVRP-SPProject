 ;(setf locations '( (0 1 3) (1 4 3) (2 5 9) (3 7 8) (4 1 3) (5 15 90) (6 1 2)  ) )
 ;(setf demands '( (0 5) (1 6) (2 7) (3 8) (4 4) (5 2) (6 1)  ) )


(in-package :user)

(defvar *current-graph*)

;;;;
; KRUSKAL MINIMUM SPANNING TREE


;; sort by the third element in a list
(defun sorter-third (x y)
  (< (third x) (third y)))
(defun rsorter-third (x y)
  (> (third x) (third y)))
;; calls member with #'equal :test
(defun memberequal (item list)
  (member item list :test #'equal))
;; slices element pos out of a list
(defun list-slice (pos l)
  (append (subseq l 0 pos)
	  (subseq l (1+ pos))))

;;;;;;;;;; union find functions

;; returns non-nil if the two vertices are in the same equiv class
(defun uf-connected (e uf)
  (member-if #'(lambda (set) (and (member (first e) set)
				  (member (second e) set)))
	     uf))
;; makes two classes equivalent by position in the union-find
(defun uf-merge (uf p1 p2)
  (if (= p1 p2) 
      uf
    (let* ((np1 (if (< p1 p2) p1 p2))
	   (np2 (if (< p1 p2) p2 p1))
	   (uf1 (nth p1 uf))
	   (uf2 (nth p2 uf)))
      (cons (union uf1 uf2) (list-slice np1 (list-slice np2 uf))))))
;; inserts an item into an equivalence class
(defun uf-insert (uf e p)
  (cons (cons e (nth p uf)) (list-slice p uf)))
;; given an old union-find returns a new union-find with the added assertion
;;  that two vertices should be in the same equivalence class. Should be
;;  called only if not already uf-connected but will work regardless.
(defun union-find (e uf)
  (let* ((e1 (first e))
	 (e2 (second e))
	 (p1 (position e1 uf :test #'memberequal))
	 (p2 (position e2 uf :test #'memberequal)))
    (cond ((and (null p1) (null p2)) (cons (list e1 e2) uf))
	  ((null p1) (uf-insert uf e1 p2))
	  ((null p2) (uf-insert uf e2 p1))
	  (t (uf-merge uf p1 p2)))))

;; Finds a minimum spanning tree, using Kruskal's algorithm.  This is
;;  *not* meant to be an efficient implementation by any means.  The idea
;;  is only that it works.  In this implementation, you may pass in
;;  #'rsorter-third and get a maximum spanning tree instead.
(defun mst (graph &optional (sorter #'sorter-third))
  (let ((sgraph (sort (copy-list graph) sorter))
	(ret nil)
	(uf nil))
    (dolist (e sgraph)
      (unless (uf-connected e uf)
	(setf uf (union-find e uf))
	(push e ret)))
    ret))

(defun mst-h (graph) (let ((num 0)) (progn 
		(dolist (x (mst graph)) (setf num (+ num (caddr x) )))

	(return-from mst-h num))
	
 )
	)

;;;;

(defstruct state
  cityID
  visited
  distance)

(defun heuristica (state)
(let ( (edges (list) ) )
			(progn 

				(dotimes (i (list-length (state-visited state) ))

					(progn 
					;put all edges from current city to depot ...
													(if (and (equalp (nth i (state-visited state)) 0)  (not (equalp (aref *current-graph* i 0) -1))) (setf edges (append edges (list (list i 0 (aref *current-graph* i 0))) )) )
													(loop for j from (1+ i) to (list-length (state-visited state) ) 
			do  
				


						(if (and (equalp (nth i (state-visited state)) 0)  (equalp (nth j (state-visited state)) 0)

			 (not (equalp (aref *current-graph* i j) -1))) 

				(setf edges (append edges (list (list i j (aref *current-graph* i j))) )) 

				) 
					

			)
						)

		
		)
(return-from heuristica  (mst-h edges))


				)

				

				)

	
	)

(defun operator (state) 
(if (not (member 0 (state-visited state)))
					;; if we're on the last city we need to check if there's a path to the depot
					(if (equalp (aref *current-graph* (state-cityID state) 0) -1) 
							(return-from operator (list))
											(return-from operator (list (make-state :cityID 0 :visited (make-list (list-length (state-visited state)) :initial-element 1) :distance (aref *current-graph* (state-cityID state) 0))))
						)


					(let ((states (list)) (temp-visited nil)) 
		(progn  
				(dotimes (i (array-dimension *current-graph* 1))
         
          (if (equalp (nth i (state-visited state)) 0)
          	(if (not (equalp (aref *current-graph* (state-cityID state) i) -1)) 

          			(progn (setf temp-visited (copy-list (state-visited state)))
          		(setf (nth i temp-visited) 1)
          	 (setf states (append states (list (make-state :cityID i :visited temp-visited :distance (aref *current-graph* (state-cityID state) i)) ))) 
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


(defun tsp-astar (problema) 
(let ((path (list))) 
	(progn 
			(dolist (state (first (procura problema "a*" :espaco-em-arvore? t)))
	 	(setf path (append path (list (state-cityID state)) ))
	  )
			(return-from tsp-astar path)
		)
	 

	))


;;;;; CLUSTER Stuff


(defun square (x) (* x x))

(defun eucledean-distance (coord1 coord2) 
	(sqrt (+ (square (- (nth 1 coord1) (nth 1 coord2))) (square (- (nth 2 coord1) (nth 2 coord2))) )))

(defstruct cluster
	capacity
	center
	points)

(defun create-clusters (capacity customer-locations customer-demands)
	(let ((clusters (list) ) (unclustered-nodes (copy-list customer-locations)))
		(progn 
			(loop while (not (equalp 0 (list-length unclustered-nodes) )) do 
				(let ((cluster (make-cluster :capacity capacity :center 0 :points (list ) )) 
						;; no problem here i think because its the furthest, but we should take depot from the unclustered nodes
						(last-node (nth (furthest-node-index (car customer-locations) unclustered-nodes) unclustered-nodes)  ) (last-node-index (furthest-node-index (car customer-locations) unclustered-nodes))  ) 
					

					(progn 
						;; on last-node-demand if we get -1 (Because no more nodes, because of nil)
					(loop while (and (not (null last-node)) (<= (node-demand last-node customer-demands) (cluster-capacity cluster)) )  do 
							(setf (cluster-points cluster) (append (cluster-points cluster) (list last-node) ))
							;; reduce capacity of cluster by demand of last-node
							(setf (cluster-capacity cluster) (- (cluster-capacity cluster) (node-demand last-node customer-demands)) )
							;; remove added point from unclustered nodes
							(setf unclustered-nodes (remove-nth last-node-index unclustered-nodes))
							;; calculate GC of cluster
							(setf (cluster-center cluster) (GC-cluster cluster) )
						    
						    (setf last-node-index (closest-node-index (cluster-center cluster) unclustered-nodes))
						    (if (null last-node-index) (setf last-node nil)
						    	(setf last-node (nth last-node-index unclustered-nodes)))
						    
						)


						(setf clusters (append clusters (list cluster) )))

					)

					

			)
			(return-from create-clusters clusters)
			)
		
	 ))

(defun copy-list-structure (some-list)
	(mapcar #'copy-structure some-list))

(defun cluster-adjustment (oldclusters customer-demands)
	(let ( (clusters (copy-list-structure oldclusters) ) (changed nil) ) 
			(dotimes (i (list-length clusters) (if (not (null changed)) clusters nil)  ) 
		(dotimes (k (list-length (cluster-points (nth i clusters)))) 
			(dotimes (j (list-length clusters)) 
					(if (and (not (null (nth k (cluster-points (nth i clusters)) ))) (not (equalp i j))
					 (<  (eucledean-distance (nth k (cluster-points (nth i clusters)) ) (cluster-center (nth j clusters)) ) (eucledean-distance (nth k (cluster-points (nth i clusters)) ) (cluster-center (nth i clusters)) ) ) 
					 (>= (cluster-capacity (nth j clusters)) (node-demand (nth k (cluster-points (nth i clusters)) ) customer-demands) ) 
					 ) 
						(progn 
								(setf changed T)
								(let ((transfer (nth k (cluster-points (nth i clusters)) )) ) 
																
								(setf (cluster-points (nth i clusters)) (remove-nth k (cluster-points (nth i clusters))) )
								(setf  (cluster-points (nth j clusters)) (append (cluster-points (nth j clusters)) (list transfer )  ) )
																
									)
								;; move k from i to j
								;; recalculate cluster center

								(setf (cluster-center (nth j clusters)) (GC-cluster (nth j clusters)) )
								(setf (cluster-center (nth i clusters)) (GC-cluster (nth i clusters)) )
							)
					)
				)
			)
		) 
		)

	)

(defun remove-nth (n list)
  (nconc (subseq list 0 n) (nthcdr (1+ n) list)))

;; always compare depot with all locations
;; consider calculating eucledean distance only once to increase performance
(defun furthest-node-index (depot customer-locations)
	(let ((max (list 0 nil)) )
		(dotimes (i  (list-length customer-locations) )
			(if (> (eucledean-distance depot (nth i customer-locations) ) (car max)) (setf max (list (eucledean-distance depot (nth i customer-locations)) i) ) )
		  )
		(return-from furthest-node-index (cadr max) )
		))




;; we return extra zero as the first element of list to be compatible with eucledean-dist func
(defun GC-cluster (cluster)
(let ( (x 0) (y 0) ) 
	(progn 
		(dolist (customer (cluster-points cluster) ) 
			(progn
				(setf x (+ x (nth 1 customer))) 
				(setf y (+ y (nth 2 customer))) 
				)
		)
		(return-from GC-cluster (list 0 (/  (coerce x 'float) (list-length  (cluster-points cluster)) ) (/  (coerce y 'float) (list-length  (cluster-points cluster)) ) ))
		)
	
	))

 ;;
 (defun closest-node-index (cluster-center customer-locations)
 	(let ((min (list nil nil)) )
		(dotimes (i  (list-length customer-locations) )
			(if (or (null (car min)) (< (eucledean-distance cluster-center (nth i customer-locations) ) (car min))) 
			 (setf min (list (eucledean-distance cluster-center (nth i customer-locations)) i) ) )
		  
		  )
		(return-from closest-node-index (cadr min) )
		))

 (defun node-demand (customer-location customer-demands)
 	(cadr (nth (car customer-location) customer-demands))
 	)

 (defun cluster-contains-depot (cluster)
 	(dolist (point (cluster-points cluster) nil)
 	 (if (equalp (car point) 0) (return-from cluster-contains-depot T))	 ))

(defun add-depot-clusters (depot clusters) 
	(let ((newclusters (copy-list-structure clusters) ))  
		(dolist (cluster newclusters newclusters) 
				(if (null (cluster-contains-depot cluster))  
						(setf (cluster-points cluster) (append (list depot) (cluster-points cluster) ) )
					)
			)
		)
	)

(defun create-graph-cluster (cluster)
	(let ((graph (make-array (list (list-length (cluster-points cluster)) (list-length (cluster-points cluster))) :initial-element 0) ) ) 
			(dotimes (i (list-length  (cluster-points cluster))  graph ) 
					(dotimes (j (list-length  (cluster-points cluster))) 
							(let ((distance  (eucledean-distance (nth i (cluster-points cluster) ) (nth j (cluster-points cluster) ) ) ))
								(setf (aref graph i j) distance )
								(setf (aref graph j i) distance )
							 )
						)
				)
		)
	)



(defun tsp-cluster (cluster)
	(let ( (graph (create-graph-cluster cluster) ) (initial-state (make-state :cityID 0 :visited (make-list (list-length (cluster-points cluster)) :initial-element 0) :distance 0)))
			(setf (car (state-visited initial-state) ) 1)
			(let ((problem (cria-problema initial-state (list 'operator) :objectivo? #'objetivo :heuristica 'heuristica :custo 'custo)))
				(setf *current-graph* graph)
				(return-from tsp-cluster (tsp-astar problem)) 
				)

		)
	) 

; returns smallest path taken from all clusters
(defun clusters-min-tsp-depot (depot clusters)
	(let ((clusters (add-depot-clusters depot clusters) ) (paths nil ) )
			(dolist (cluster clusters paths)
				(let ((result (tsp-cluster cluster)))
						(if (or (null paths) (<= (list-length result) (list-length paths) )) 
					 (setf paths result))
					)

					
					)

				)
		)

; returns all paths taken inside each cluster ( each cluster already contains the depot)
(defun clusters-tsp-depot (depot clusters)
	(let ((clusters (add-depot-clusters depot clusters) ) (paths (list) ) )
			(dolist (cluster clusters paths) 
				 (setf paths (append paths (list (tsp-cluster cluster) ) ))	
				)
		))

; algorithm with cluster adjustment
(defun clusters-tsp-depot-adjustment (depot clusters customer-demands)
	(let ( (clusters (copy-list clusters) ) (depot-clusters (add-depot-clusters depot clusters) ) (paths (list) ) )
			(dolist (cluster depot-clusters) 
				 (setf paths (append paths (list (tsp-cluster cluster) ) ))	
				)

			(loop while (not (null (setf clusters (cluster-adjustment clusters customer-demands) ) )  ) do 
					(setf depot-clusters (add-depot-clusters depot clusters) )
					(dolist (cluster depot-clusters) 
				 (setf paths (append paths (list (tsp-cluster cluster) ) ))	
				)

				)
			(return-from clusters-tsp-depot-adjustment paths)
		))



(defstruct vrp
  name
  vehicle.capacity
  vehicles.number
  max.tour.length
  customer.locations
  customer.demand)
;; vehicles number - max numbers of tours
;; name is simply name of the instance of the problem
;; total tour length does not exceed max.tour.length
;; total demand does not exceed vehicle capacity
;; customer locations is a list of x,y pairs identified by ids. (id, x, y)
;; customer demands is a list of demands identified by ids. (id, demand)

(defun vrp (problem heuristic)
	(clusters-tsp-depot-adjustment (nth 0 (vrp-customer.locations problem))
	 (create-clusters (vrp-vehicle.capacity problem) (vrp-customer.locations problem) (vrp-customer.demand problem))  (vrp-customer.demand problem))
	)


(setf problem (make-vrp :NAME "CMT1"
	:VEHICLE.CAPACITY 30
	:VEHICLES.NUMBER 5
	:MAX.TOUR.LENGTH 150
	:CUSTOMER.LOCATIONS '( (0 1 3) (1 4 3) (2 5 9) (3 7 8) (4 1 3) (5 15 90) (6 1 2)  )
	:CUSTOMER.DEMAND '( (0 5) (1 6) (2 7) (3 8) (4 4) (5 2) (6 1)  ) ))
