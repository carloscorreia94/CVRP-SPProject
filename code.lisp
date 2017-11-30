(in-package :user)

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


;;;; RETURNS

;; listh with (vehicles.number) sequence of nodes visited in form (0...0) 
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
						(last-node (nth (furthest-node-index (car locations) unclustered-nodes) unclustered-nodes)  ) (last-node-index (furthest-node-index (car locations) unclustered-nodes))  ) 
					

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

(defun cluster-adjustment (oldclusters customer-demands)
	(let ( (clusters (copy-list oldclusters) ) ) 
			(dotimes (i (list-length clusters) clusters) 
		(dotimes (k (list-length (cluster-points (nth i clusters)))) 
			(dotimes (j (list-length clusters)) 
					(if (and (not (equalp i j))
					 (<  (eucledean-distance (nth k (cluster-points (nth i clusters)) ) (cluster-center (nth j clusters)) ) (eucledean-distance (nth k (cluster-points (nth i clusters)) ) (cluster-center (nth i clusters)) ) ) 
					 (>= (cluster-capacity (nth j clusters)) (node-demand (nth k (cluster-points (nth i clusters)) ) customer-demands) ) 
					 ) 
						(progn 
							(print "SHOULD!")
								;; move k from i to j
								(setf  (cluster-points (nth j clusters)) (append (cluster-points (nth j clusters)) (list (nth k (cluster-points (nth i clusters)) ) ) ) )
								(setf (cluster-points (nth i clusters)) (remove-nth k (cluster-points (nth i clusters))) )
								;; recalculate cluster center

								(setf (cluster-center (nth j clusters)) (GC-cluster (nth j clusters)) )
								(setf (cluster-center (nth i clusters)) (GC-cluster (nth j clusters)) )
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


 (setf locations '( (0 1 3) (1 4 3) (2 5 9) (3 7 8) (4 1 3) (5 15 90) (6 1 2)  ) )
 (setf demands '( (0 5) (1 6) (2 7) (3 8) (4 4) (5 2) (6 1)  ) )

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

