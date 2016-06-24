
(in-package :cl-user)

(defun build-disjoints ()
  (write-disjoints (load-kb-jena "obi:newids;obil.owl") "obi:newids;disjoints.owl"))

(defun write-disjoints (&optional (kb (load-kb-jena :obi)) (path "obi:branches;disjoints.owl"))
  (loop 
     with parent2child = (make-hash-table)
     with labels = (make-hash-table)
     for (sub super subname supername) in
     ;; query to retrieve all stated parent child relationships and their labels
     (sparql '(:select (?sub ?super ?subname ?supername) (:distinct t)
	       (?sub !rdfs:subClassOf ?super)
	       (:optional (?sub !rdfs:label ?subname))
	       (:optional (?super !rdfs:label ?supername))
	       (:filter (not (or (isblank ?sub) (isblank ?super)))))
	     :kb kb
	     :use-reasoner :none)
     do
     ;; build the asserted hierarchy tree as a hash table mapping parent to children
     (pushnew sub (gethash super parent2child))

     ;; build a cache of labels as a hash mapping uri to label
     (setf (gethash sub labels) subname)
     (setf (gethash super labels) supername)

     ;; iterate downward from bfo:Entity breadth first using a
     ;; queue. At each iteration handle the root (first in queue) and
     ;; append and children of the root that we want to process to
     ;; the end. Repeat until queue empty.

     ;; If you hit a placeholder class stop otherwise add mutual
     ;; disjoints between the obi classes, and disjoints from them to
     ;; the other siblings BUT don't add a disjoint from a defined
     ;; class or placeholder class to anything else.

     finally
     (loop with queue = (list !bfo:Entity) 
	with defined-classes = (defined-classes kb) ; i.e. those with necessary and sufficient conditions
	with placeholders = (placeholder-classes kb) ; in obi, those classes prepended by a "_"
	with all-disjoints = nil ; accumulating list of disjoint-classes assertion
	for root = (pop queue)
	for children = (gethash root parent2child)
	for obi-children = (remove-if-not 'is-obi-uri children)
	for other-children = (set-difference (set-difference children obi-children) placeholders)
	;; don't bother doing anything unless there is something to do
	when (and obi-children
		  (or (eq root !material-entity) ; special case - we add disjoints below material-entity
		      (and (not (member root defined-classes)) ; but not defined
			   (not (member root placeholders))
			   (not (member root `(,!snap:Object ,!snap:ObjectAggregate ,!snap:FiatObjectPart)))
			   ))) ; or placeholder
	do (progn
	     ;; all the obi-only classes are mutually disjoint
	     (let ((obi-disjoints (set-difference (set-difference obi-children placeholders) defined-classes)))
;	       (if (member !obi:OBI_0000233 obi-disjoints :test 'equalp) (break))
	       (when (>= (length obi-disjoints) 2)
		 (push `(disjoint-classes ,@obi-disjoints) all-disjoints))
	       ;; all the obi versus other siblings are pairwise
	       ;; disjoint (other than defined classes - placeholders
	       ;; already removed)
	       (dolist (other other-children)
		 (dolist (obi obi-children)
		   (unless (or (member obi defined-classes)
			       (member obi placeholders)
			       (member other defined-classes)
			       (not (member root `(,!snap:Object ,!snap:ObjectAggregate ,!snap:FiatObjectPart))))
		     (push `(disjoint-classes ,obi ,other) all-disjoints))))))
	;; don't into placeholder classes
	;; descend into defined classes because otherwise we never reach anything.
	do (setf queue (append queue obi-children other-children)) 
	until (null queue)
	finally
	;; create and write the file full of disjoints
	(eval `(with-ontology disjoints (:base "http://purl.obolibrary.org/obo/obi/disjoints.owl")
		   ,all-disjoints
		 (write-rdfxml disjoints ,path)))
	))
  ;; return the path for convenience - reminder of where to look if you want to inspect
  (namestring (truename path)))

(defun defined-classes (&optional (kb (load-kb-jena :obi)))
  "find defined classes by looking for equivalent class statements to blank nodes"
  (sparql '(:select (?class) (:distinct t)
	    (?class !owl:equivalentClass ?other)
	    (:filter (and (isblank ?other))))
	  :kb kb
	  :use-reasoner :none
	  :flatten t))

(defun placeholder-classes (&optional (kb (load-kb-jena :obi)))
  "find placeholder classes, which in OBI are those that are named with an '_' as the first character"
  (loop for (class name) in 
       (sparql '(:select (?class ?label) (:distinct t)
		 (?class !rdfs:label ?label)
		 (?class !rdf:type !owl:Class)
		 ;; (:filter (regex ?label "^_")) ;; bug!! Doesn't match when it should. Fencepost?
		 )
	       :kb kb
	       :use-reasoner :none)
       when (and name (char= (char name 0) #\_))
       collect class
       ))