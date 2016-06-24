;; (defparameter *obi-prefixes*
;;   '(("xsd" "http://www.w3.org/2001/XMLSchema#")
;;     ("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
;;     ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")
;;     ("owl" "http://www.w3.org/2002/07/owl#")
;;     ("daml" "http://www.daml.org/2001/03/daml+oil#")
;;     ("dcterms" "http://purl.org/dc/terms/")
;;     ("dc" "http://purl.org/dc/elements/1.1/")
;;     ("protege" "http://protege.stanford.edu/plugins/owl/protege#")
;;     ("protege-dc" "http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl#")
;;     ("oboInOwl" "http://www.geneontology.org/formats/oboInOwl#")
;;     ("bfo" "http://www.ifomis.org/bfo/1.1#")
;;     ("robfo" "http://purl.org/obo/owl/ro_bfo_bridge/1.1#")
;;     ("snap" "http://www.ifomis.org/bfo/1.1/snap#")
;;     ("span" "http://www.ifomis.org/bfo/1.1/span#")
;;     ("ro" "http://www.obofoundry.org/ro/ro.owl#")
;;     ("rotoo" "http://purl.org/obo/owl/ro#")
;;     ("pato" "http://purl.org/obo/owl/PATO#")
;;     ("cell" "http://purl.org/obo/owl/CL#")
;;     ("chebi" "http://purl.org/obo/owl/CHEBI#")
;;     ("envo""http://purl.org/obo/owl/ENVO#")
;;     ("ncbitax""http://purl.org/obo/owl/NCBITaxon#")
;;     ("obi" "http://purl.obolibrary.org/obo/")
;;     ("caro" "http://purl.org/obo/owl/CARO#")
;;     ("pro" "http://purl.org/obo/owl/PRO#")
;;     ("so" "http://purl.org/obo/owl/SO#")
;;     ("go" "http://purl.org/obo/owl/GO#")
;;     ("obi_denrie" "http://purl.obolibrary.org/obo/obi/DigitalEntityPlus.owl#")
;;     ("obi_biomat" "http://purl.obolibrary.org/obo/obi/Biomaterial.owl#")
;;     ("obi_extd" "http://purl.obolibrary.org/obo/obi/externalDerived.owl#")
;;     ("obi_rel" "http://purl.obolibrary.org/obo/obi/Relations.owl#")
;;     ("obi_plan" "http://purl.obolibrary.org/obo/obi/PlanAndPlannedProcess.owl#")
;;     ("obi_role" "http://purl.obolibrary.org/obo/obi/Role.owl#")
;;     ("obi_instr" "http://purl.obolibrary.org/obo/obi/InstrumentAndPart.owl#")
;;     ("obi_func" "http://purl.obolibrary.org/obo/obi/OBI-Function.owl#")
;;     ("obi_annot" "http://purl.obolibrary.org/obo/obi/AnnotationProperty.owl#")
;;     ("obi_ext" "http://purl.obolibrary.org/obo/obi/external.owl#")
;;     ("obi_quality" "http://purl.obolibrary.org/obo/obi/Quality.owl#")
;;     ("obi_owlfull" "http://purl.obolibrary.org/obo/obi/obi-owl-full.owl#")))
;; (print "I'm here")
;; (setq *sparql-always-trace* t)
;; (trace get-url)
;; (defun sparql (query &rest all &key (kb (and (boundp '*default-kb*) *default-kb*)) (use-reasoner *default-reasoner*) (flatten nil) (trace nil) (trace-show-query t) endpoint-options geturl-options (values t) (endpoint nil) (chunk-size nil) (syntax :sparql) &allow-other-keys &aux (command :select) count)
;;   (when chunk-size (return-from sparql (apply 'sparql-by-chunk query all)))
;;   (setq use-reasoner (or endpoint use-reasoner))
;;   (setq count (and (consp query)
;; 		   (eq (car query) :select)
;; 		   (getf (third query) :count)
;; 		   (member use-reasoner '(:jena :none :pellet :sparqldl))))
;;   (when (typep kb 'owl-ontology)
;;     (setq kb (kb kb))) 
;;   (when (listp query) 
;;     (setq command (car query))
;;     (setq query (sparql-stringify query use-reasoner)))
;;   (setq use-reasoner (or (second (assoc use-reasoner *endpoint-abbreviations*)) use-reasoner))
;;   (if (stringp use-reasoner) (setq use-reasoner (make-uri use-reasoner)))
;;   (let ((do-trace (or *sparql-always-trace* (and trace  *sparql-allow-trace*))))
;;     (if (and do-trace trace-show-query)
;;       (format t "Query: ~a~%~a~%Results:~%" (or trace "Tracing all")  query)
;;       (if do-trace
;; 	  (format t "Query: ~a~%Results:~%" (or trace "Tracing all"))))
;;     (if (uri-p use-reasoner)
;; 	(let ((bindings (sparql-endpoint-query use-reasoner query :query-options endpoint-options :geturl-options geturl-options :command command)))
;; 	  (when do-trace
;; 	    (loop for one in bindings
;; 	       do (format t "~{~s~^	~}~%" one))
;; 	    (terpri t))
;; 	  (if flatten (loop for b in bindings append b) (if values bindings (values))))

;; 	(let* (	;; Query query = QueryFactory.create(queryString);
;; 	       (jquery (#"create" 'QueryFactory query (if (eq syntax :terp)
;; 							  (#"getInstance" 'TerpSyntax)
;; 							  (#"lookup" 'Syntax "SPARQL"))))


;; 	       ;; Execute the query and obtain results
;; 	       ;; QueryExecution qe = QueryExecutionFactory.create(query, model);
;; 	       (qe (cond ((or (member use-reasoner '(:sparqldl :pellet t)))
;; 			  (unless (v3kb-pellet-jena-model kb)
;; 			    (instantiate-reasoner kb :pellet-sparql nil)
;; 			    (unless (v3kb-pellet-jena-model kb)
;; 			      (setf (v3kb-pellet-jena-model kb) 
;; 				     (let ((graph (new 'org.mindswap.pellet.jena.PelletReasoner)))
;; 				       (#"createInfModel" 'com.hp.hpl.jena.rdf.model.ModelFactory
;; 							  (#"bind" graph (#"getKB" (v3kb-reasoner kb))))))))
;; 			  (#"prepare" (v3kb-pellet-jena-model kb))
;; 			  (#"create" 'SparqlDLExecutionFactory jquery (v3kb-pellet-jena-model kb)))
;; 			 ((or (eq use-reasoner :none) (eq use-reasoner nil)) 
;; 			  (#"create" 'QueryExecutionFactory jquery
;; 				     (if (java-object-p kb) kb (jena-model kb))))
;; 			 ((or (eq use-reasoner :jena))
;; 			  (unless (v3kb-pellet-jena-model kb) (instantiate-reasoner kb :pellet-sparql nil))
;; 			  (#"create" 'QueryExecutionFactory jquery (v3kb-pellet-jena-model kb)))
;; 			 ((eq use-reasoner :owl) (error "Not supported yet")
;; 			  (#"create" 'QueryExecutionFactory jquery 
;; 				     (#"createInfModel" 'modelfactory 
;; 							(#"getOWLReasoner" 'ReasonerRegistry)
;; 							(#"getModel" (kb-jena-reasoner kb)))))))
;; 	       (print-db qe)
;; 	       ;; ResultSet results = qe.execSelect();
;; 	       (vars (set-to-list (#"getResultVars" jquery))))
;; 	  (unwind-protect
;; 	       (with-constant-signature ((getv "get") (next "next") (has-next "hasNext") (get-uri "getURI"))
;; 		 (flet ((get-vars (bindingset)
;; 			  (let ((bindings
;; 				 (loop for var in vars 
;; 				    for jval = (getv bindingset var)
;; 				    for val = (if (null jval) 
;; 						  nil
;; 						  (if 
;; 						   (#"isResource" jval)
;; 						   (make-uri (or (get-uri jval)
;; 								 (format nil "~a~a" *blankprefix* (#"toString" jval))
;; 								 ))
;; 						   (#"getValue" jval)))
;; 				    collect val)))
;; 			    (when do-trace
;; 			      (format t "~{~s~^	~}~%" bindings))
;; 			    bindings)))
;; 					;		 (when (and (eq use-reasoner :pellet) query-uses-blank-nodes)
;; 					;		   (set-java-field 'PelletOptions "TREAT_ALL_VARS_DISTINGUISHED" nil))
;; 		   ;; (if (member use-reasoner '(:pellet :jena))
;; ;; 		       (when (kb-kb kb)
;; ;; 			 (#"realize" (kb-kb kb))))
;; 					; work around pellet bug
;; 		   (let ((results (if (eq use-reasoner :pellet)
;; 				      (#"execSelect" qe) ; (#"execQuery" (v3kb-jena-reasoner kb) jquery)

;; 				      (#"execSelect" qe))))

;; 		     (if count (return-from sparql (loop while (has-next results) do (next results) sum 1)))
;; 		     (if values 
;; 			 (if flatten 
;; 			     (loop while (has-next results) 
;; 				append
;; 				(get-vars (next results)))
;; 			     (loop while (has-next results) 
;; 				collect 
;; 				(get-vars (next results))))
;; 			 (loop while (has-next results) 
;; 			      do (get-vars (next results)) finally (return (values))
;; 			    )
;; 			 ))))

;; 	    ;; Important - free up resources used running the query
;; 	    ;; qe.close();
;; 	    (#"close" qe)
;; 	    (if do-trace (terpri))
;; 	    )))))

;; (defun create-external-derived (&key
;; 				(kb (load-kb-jena "obi:branches;external.owl"))
;; 				(templates-path "obi:lisp;external-templates.txt")
;; 				(output-path (merge-pathnames
;; 					      "externalDerived.owl"
;; 					      (truename "obi:branches;")))

;; 				(endpoint nil)
;; 				(debug t)
;; 				(ontology-uri "http://purl.obolibrary.org/obo/obi/externalDerived.owl"))
;;   (let ((*sparql-always-trace* (or *sparql-always-trace* debug)))
;;     (declare (special *sparql-always-trace*))
;;     (let ((terms 
;; 	   (sparql '(:select (?term ?where ?parent) () 
;; 					;(?term !rdf:type !owl:Class)
;; 		     (?term !obi:IAO_0000412 ?where)
;; 		     (:union ((?term !rdfs:subClassOf ?parent)) ((?term !rdf:type ?parent) (:filter (not (equal ?parent !owl:Class)))))
;; 		     )
;; 		   :use-reasoner :none ;; turn the reasoner off, so that we don't get the obi superclasses
;; 		   :kb kb :trace t))
;; 	  (classes 
;;       	   (sparql '(:select (?term) () 
;; 		     (?term !rdf:type !owl:Class))
;; 		   :use-reasoner :none ;; turn the reasoner off, so that we don't get the obi superclasses
;; 		   :kb kb :trace t))
;; 	  (instances 
;; 	   (sparql '(:select (?term ?type) () 
;; 		     (?term !rdf:type ?type)
;; 		     (:filter (and (not (regex (str ?type) "^http://www.w3.org/2002/07/owl#")))))
;; 		   :use-reasoner :none ;; turn the reasoner off, so that we don't get the obi superclasses
;; 		   :kb kb :trace t)))
;;       (format t "There are ~a external terms - ~a classes and ~a instances~%" (length terms) (- (length terms) (length instances)) (length instances))
;;       (multiple-value-bind (params templates) (parse-templates templates-path)
;; 	(let ((endpoint (or (second (assoc "default endpoint" params :test 'equal)) endpoint))
;; 	      (graph_base (second (assoc "default graph_base" params :test 'equal))))
;; 	  (assert endpoint () "What endpoint should I use?")
;; 	  (format t "Using endpoint: ~a~%" endpoint)
;; 	  (let ((rdfs 
;; 		 (append
;; 		  (loop for query in (cadr (assoc "Once Only" templates :test 'equalp))
;; 		     collect (get-url endpoint :post `(("query" ,query)) :persist nil :dont-cache t :force-refetch t))
;; 		  (loop for (term where) in terms
;; 		     for graph = (#"replaceAll" (uri-full where) ".*[/#](.*?)(\.owl){0,1}" "$1")
;; 		       do (print-db term where graph)
;; 		     ;; when debug do (print-db graph)
;; 		     append
;; 		     (loop for (ont-pattern queries) in templates
;; 			  for matches = (#"matches" (uri-full where)  (format nil "(?i)~a" ont-pattern))
;; 			for whichma = (progn (print-db (uri-full where)  (format nil "(?i)~a" ont-pattern) matches) nil)
;; 			when matches
;; 			append
;; 			(loop for query in queries
;; 			   for filled-query = 
;; 			   (#"replaceAll" (#"replaceAll" query "_ID_GOES_HERE_" (format nil "<~a>" (uri-full term))) "_GRAPH_GOES_HERE_" (format nil "<~a~a>" graph_base (#"replaceAll" (string-upcase graph) ".OWL$" "")))
;; 			     do (when debug
;; 				     (progn
;; 				       (print-db (uri-full term))
;; 				       (print-db filled-query)
;; 				       (print (get-url endpoint :post `(("query" ,filled-query)) :persist nil :dont-cache t :force-refetch t))))

;; 			   ;; FIXME - Horrible workaround to compensate for abcl not understanding unicode and VO using smart quotes
;; 			   ;; Explain to me why the get URL produces different strings on a mac compared to a linux box. Byte order?
;; 			   collect (xml-encode-unicode-high (setq foo (#"replaceAll" (#"replaceAll" (setq bar (get-url endpoint :post `(("query" ,filled-query)) :persist nil :dont-cache t :force-refetch t)) *smart-leftquote-pattern* "&#8216;") *smart-rightquote-pattern* "&#8217;")))))))))
;; 	    (let ((basic-info
;; 		   (with-output-to-string (s)
;; 		     (write-string "<?xml version=\"1.0\" encoding=\"utf-8\" ?>" s) (terpri s)
;; 		     (write-string "<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\">" s) (terpri s)
;; 		     (loop for (class) in (intersection classes terms)
;; 			do (format s "<owl:Class rdf:about=~s></owl:Class>~%"
;; 				   (uri-full class) ))
;; 		     (loop for (instance type) in (intersection instances terms)
;; 			do (format s "<rdf:Description rdf:about=~s><rdf:type rdf:resource=~s/></rdf:Description>~%"
;; 				   (uri-full instance) (uri-full type) )
;; 			  )
;; 		     (write-string "</rdf:RDF>" s)
;; 		     )))
;; 	      (combine-template-query-results (cons basic-info rdfs) output-path ontology-uri))
;; 	    (clean-rdf (namestring (truename output-path)) *obi-prefixes* ontology-uri)
;; 	    nil
;; 	    ))))))