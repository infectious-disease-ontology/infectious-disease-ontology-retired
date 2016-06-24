(defparameter *core-classes* 
  (remove-duplicates(list 
!snap:MaterialEntity ;;material entity
	!obo:OBI_0100026 ;;organism
	!obo:OBI_0000245 ;;organization
	!chebi:23367	 ;;molecular entities
	!obo:OBI_0302729 ;;chemical entities in solution
	;;!obo:OBI_0000256 ;;environmental matter - tossed for now
	!obo:OBI_0100015 ;;anatomical entity
	!obo:OBI_0000047 ;;processed material - was artifact object
	;;!obo:OBI_0100051 ;;specimen



	!obo:IAO_0000027 ;;data item
	!obo:IAO_0000100 ;;data set
	!obo:OBI_0000658 ;;data structure (data representational model)
	!obo:IAO_0000091 ;;narrative object - report
	!obo:IAO_0000059 ;;narrative object - figure
	!obo:IAO_0000010 ;;software
	!obo:IAO_0000010 ;;controlled variable specification
	!obo:IAO_0000014 ;;dependent variable specification
	!obo:IAO_0000011 ;;independent variable specification
	!obo:OBI_0000074 ;;hypothesis
	!obo:IAO_0000088 ;;report of results
	!obo:IAO_0000035 ;;conclusion
	!obo:IAO_0000005 ;;objective specification
	!obo:OBI_0500000 ;;study design
	!obo:IAO_0000032 ;;scalar measurement datum

	!obo:OBI_0000275 ;;analyte role
	!obo:OBI_0000587 ;;protocol participant role
	!obo:OBI_0000022 ;;specified input role
	!obo:OBI_0000657 ;;specified output role
	!obo:OBI_0000067 ;;evaluant role
	!obo:OBI_0000086 ;;reagent role
	!obo:OBI_0000133 ;;reference role
	!obo:OBI_0000202 ;;investigation agent role - was study personnel role
	!obo:OBI_0000097 ;;study subject role
	!obo:OBI_0000112 ;;specimen role
	!obo:OBI_0000111 ;;study participant role 





	!obo:OBI_0000274 ;;adding a material entity into a target (was material administration?)
	!obo:OBI_0000443 ;;analyte assay
	!obo:OBI_0000652 ;; material combination
	!obo:OBI_0600014 ;;material separation
	!obo:OBI_0000011 ;;planned process
	!obo:OBI_0000094 ;;processing material - was artifact creation?
	!obo:OBI_0000471 ;;study design execution
	!obo:OBI_0000070 ;;assay
	!obo:OBI_0000457 ;;manufacturing
	!obo:OBI_0000339 ;;planning
	!obo:OBI_0000340 ;;documenting
	!obo:OBI_0500000 ;;study design - already in from denrie
	!obo:OBI_0000272 ;;protocol
	!obo:OBI_0000066 ;;investigation
	!obo:IAO_0000005 ;;objective specification - already in from DENRIE
	!obo:IAO_0000104 ;;plan specification
	!obo:OBI_0000441 ;;assay objective
	!obo:OBI_0000437 ;;analyte measuring objective
	!obo:OBI_0000456 ;;material transformation objective
	!obo:OBI_0000458 ;;manufacturing objective
	!obo:OBI_0000686 ;;material combination objective
	!obo:OBI_0000434 ;;adding material objective
	!obo:OBI_0000639 ;;material separation objective


	!obo:OBI_0200000 ;;DT
	!obo:OBI_0200166 ;;DT objective
	!obo:IAO_0000027 ;;data item - already in from DENRIE
	!obo:IAO_0000100 ;;data set - already in from DENRIE
	;;report figure -  - already in from DENRIE
	;;software -  - already in from DENRIE



	!obo:OBI_0400002 ;;device
	!obo:OBI_0400003 ;;instrument
	!obo:OBI_0000050 ;;platform
	!obo:OBI_0000047 ;;processed material - was artifact object - already in from Biom
	!obo:OBI_0400167 ;;device function
	!obo:OBI_0000402 ;;canonical realization of device function
	!obo:OBI_0000453 ;;produce data function
	!obo:OBI_0000392 ;;information processor function
	!span:Process
	!obo:OBI_0000272 ;;protocol - already in from PaPP
	)))

(defparameter *core-properties*
  (list
   !obo:OBI_0000417 ;; achieves_planned_objective
   !obo:OBI_0000301 ;;has_specified_output_information
   !obo:OBI_0000315 ;;has_specified_input_information
   ))


;(setq kb (load-kb-jena "~/repos/obi/releases/2009-03-10/branches/obil.owl"))
; doesn't write core properties yet.
; doesn't write axioms, either real or as text.
(defun write-core-terms (kb &optional (dest "obi:branches;core.owl"))
  (let ((*default-kb* kb)
	(supers (make-hash-table))
	(queue *core-classes*)
	(annotation-properties nil))
    (labels ((do-parents (term parents)
	       (let* ((dont-show-parents (remove-if-not
					  (lambda(e) (#"matches"  (car (rdfs-label e kb)) "^_.*"))
					  (remove !owl:Thing parents)))
		      (show-parents (set-difference parents dont-show-parents)))
		 (if dont-show-parents 
		     (do-parents term (union show-parents (apply 'append (mapcar 'parents dont-show-parents))))
		     (dolist (one show-parents)
		       (pushnew one (gethash term supers))
		       (push one queue)))))
	     (remember-annotation-property (p) (pushnew p annotation-properties)))
      (loop for term = (pop queue)
	 do
	   (unless (eq term !owl:Thing)
	     (do-parents term (or (parents term) (list !owl:Thing))))
	 while queue
	 finally 
	 (with-ontology obi-core (:base (uri-full !obi:) :about (uri-full !obi:obi.owl))
	     ((ontology-annotation !<http://protege.stanford.edu/plugins/owl/protege#defaultLanguage> "en")
	      (annotation-property !<http://protege.stanford.edu/plugins/owl/protege#defaultLanguage>)
	      (loop for sub being the hash-keys of supers 
		 using (hash-value supers)
		 collect (apply 'class sub :partial 
				(append (loop for (p v) in (annotation-property-values-or-labels sub)
					     do (remember-annotation-property p)
					   collect (annotation p v))
					(loop for label in (rdfs-label sub kb)
					     collect (label label))
				supers)
				))
	      (loop for p in annotation-properties collect (annotation-property p (label (car (rdfs-label p kb)))))
	     
	      )
	   (show-classtree obi-core :depth 10)
	   (write-rdfxml obi-core dest)
	   (return-from write-core-terms supers))
	 ))))



       
