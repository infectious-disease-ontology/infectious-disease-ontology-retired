(defun owl-version-info-report (kb)
  (sparql
   '(:select (?si ?label) (:distinct t)
     (?si !owl:versionInfo ?label)
     (:optional (?si :a ?type))
     (:filter 
	    (not(equal ?type !owl:Ontology))
	       )
     )
   :kb kb :use-reasoner :jena :trace "owl:VersionInfo" :values nil :trace-show-query nil))


(defun rdfs-class-report (kb)
  (sparql
   '(:select (?si ?label) (:distinct t)
     (?si !rdfs:label ?label)
     (?si :a ?type)
     (:filter 
	       (equal ?type !rdfs:Class)
	       )
     )
   :kb kb :use-reasoner :jena :trace "rdfs:Class" :values nil :trace-show-query nil))

(defun curation-status-report (kb)
  (sparql
   '(:select (?s ?status) (:distinct t)
     (?p !rdfs:label |\"has curation status\"@en|)
      (:optional
       (?si ?p ?statusi)
       (?statusi !rdfs:label ?status)
       (?si !rdfs:label ?s))
     (:optional
      (?si ?p ?status)
      (:filter (isLiteral ?status))
      (?si !rdfs:label ?s)
      )
     )
   :kb kb :use-reasoner :none :trace "Curation status" :values nil :trace-show-query nil))

(defun missing-curation (kb)
  (sparql
   '(:select (?si ?s ?type) (:distinct t)
     (?p !rdfs:label |\"has curation status\"@en|)
     (?p :a !owl:AnnotationProperty)
     (:union
      ((?si !rdf:type !owl:AnnotationProperty))
      ((?si !rdf:type !owl:ObjectProperty))
      ((?si !rdf:type !owl:DatatypeProperty))
      ((?si !rdf:type !owl:Class))
     )
     (?si !rdfs:label ?s)
     (?si :a ?type)
     (:optional (?si ?p ?status))
     (:filter (and (regex (str ?si) "obi|OBI")
	       (not (regex (str ?s) "^obsolete"))
	       (not (equal ?type !rdf:Property))
	       (not (equal ?type !owl:Thing))
	       (not (equal ?type !rdfs:Class))
	       (not (bound ?status))))
     )
   :kb kb :use-reasoner :none :trace "Terms missing curation status" :values nil :trace-show-query nil))

(defun extra-curation-status-instances (kb)
  (sparql
   '(:select (?ci ?label) (:distinct t)
     (?c !rdfs:label |\"has curation status\"@en|)
     (?c :a !owl:Class)
     (?ci :a ?c)
     (:optional (?ci !rdfs:label ?label))
     (:filter (and 
	       (not (equal ?ci !ready-for-release))
	       (not (equal ?ci !metadata-complete))
	       (not (equal ?ci !metadata-incomplete))
	       (not (equal ?ci !pending-final-vetting))
               (not (equal ?ci !example-to-be-eventually-removed))
	       (not (equal ?ci !uncurated))
	       (not (equal ?ci !placeholder)) 
	       )))
   :kb kb :use-reasoner :none :trace "Extra curation status instances" :values nil :trace-show-query nil))
  

;; (defun string-curation-status (kb)
;;   (sparql
;;    '(:select (?si ?s ?type) (:distinct t)
;;      (?p !rdfs:label |\"curation status\"@en|)
;;      (?p :a !owl:AnnotationProperty)
;;      (:union
;;       ((?si !rdf:type !owl:AnnotationProperty))
;;       ((?si !rdf:type !owl:ObjectProperty))
;;       ((?si !rdf:type !owl:DatatypeProperty))
;;       ((?si !rdf:type !owl:Class))
;;      )
;;      (?si !rdfs:label ?s)
;;      (?si :a ?type)
;;      (:optional (?si ?p ?status))
;;      (:filter (and (regex (str ?si) "obi|OBI")
;; 	       (not (equal ?type !rdf:Property))
;; 	       (not (equal ?type !owl:Thing))
;; 	       (not (equal ?type !rdfs:Class))
;; 	       (not (bound ?status))
;; 	       (not (isuri ?status))))
;;      )
;;    :kb kb :use-reasoner :none :trace "Terms with strings as curation status" :values nil :trace-show-query nil))

(defun untranslated-uris (kb)
  (sparql
   '(:select (?si ?s) (:distinct t)
     (?si !rdfs:label ?s)
     (:union
      ((?si !rdf:type !owl:AnnotationProperty))
      ((?si !rdf:type !owl:ObjectProperty))
      ((?si !rdf:type !owl:DatatypeProperty))
      ((?si !rdf:type !owl:Class))
     )
     (:filter (and (regex (str ?si) "obi|OBI") (not (regex (str ?si) "http://purl.obolibrary.org") ))
     ))
   :kb kb :use-reasoner :jena :trace "old obi ids" :values nil :trace-show-query nil)
  (sparql
   '(:select (?si ?s) (:distinct t)
     (?si !rdfs:label ?s)
     (:union
      ((?si !rdf:type !owl:AnnotationProperty))
      ((?si !rdf:type !owl:ObjectProperty))
      ((?si !rdf:type !owl:DatatypeProperty))
      ((?si !rdf:type !owl:Class))
     )
     (:filter (regex (str ?si) "http://www.geneontology.org/formats/oboInOwl#")))
   :kb kb :use-reasoner :jena :trace "geneontology ids" :values nil :trace-show-query nil)
  (sparql
   '(:select (?si ?s) (:distinct t)
     (?si !rdfs:label ?s)
     (:union
      ((?si !rdf:type !owl:AnnotationProperty))
      ((?si !rdf:type !owl:ObjectProperty))
      ((?si !rdf:type !owl:DatatypeProperty))
      ((?si !rdf:type !owl:Class))
     )
     (:filter (regex (str ?si) "Class_\\d+")))
   :kb kb :use-reasoner :jena :trace "ids that look like Class_12" :values nil :trace-show-query nil)
  (sparql
   '(:select (?si ?s) (:distinct t)
     (?si !rdfs:label ?s)
     (:union
      ((?si !rdf:type !owl:AnnotationProperty))
      ((?si !rdf:type !owl:ObjectProperty))
      ((?si !rdf:type !owl:DatatypeProperty))
      ((?si !rdf:type !owl:Class))
      )
     (:filter (and (regex (str ?si) "http://purl.obolibrary.org")
	       (not (regex (str ?si) "http://purl.obolibrary.org/obo/OBI_\\d{7}") )
 (not (regex (str ?si) "http://purl.obolibrary.org/obo/IAO_\\d{7}") )))
      )
   :kb kb :use-reasoner :jena :trace "uris without ids assigned yet" :values nil :trace-show-query nil)
  )

(defun next-unused-id (kb &optional (howmany 1))
  (let ((already
	 (mapcar (lambda(uri)
		   (parse-integer
		    (subseq (uri-full uri)
			    (+ 4 (search "OBI_" (uri-full uri))))))
		 (sparql
		  '(:select (?si) (:distinct t)
		    (?si !rdf:type ?type)
		    (:filter (regex (str ?si) "OBI_\\d+")))
		  :kb kb :use-reasoner :jena :flatten t))))
    (loop for candidate from 1 
       with count = 1
       when (not (member candidate already))
       collect candidate into nexts and do (incf count)
       do
	 (when (> count howmany)
	   (return-from next-unused-id nexts)))))

(defun missing-label (kb)
  (let ((missing 
	 (sparql
	  '(:select (?si) (:distinct t)
	    (:union
	     ((?si !rdf:type !owl:AnnotationProperty))
	     ((?si !rdf:type !owl:ObjectProperty))
	     ((?si !rdf:type !owl:DatatypeProperty))
	     ((?si !rdf:type !owl:Class))
	     )
	    (:optional (?si !rdfs:label ?label))
	    (?si :a ?type)
	    (:filter (and (regex (str ?si) "(http://purl.obolibrary.org/)|obi|OBI")
		      (not (equal ?type !rdf:Property))
		      (not (equal ?type !owl:Thing))
		      (not (equal ?type !rdfs:Class))
		      (not (bound ?label))))
	    )
	  :kb kb :use-reasoner :jena :flatten t)))
    (when missing
      (loop for el in missing do
	   (format t "~a ~{~%  ~a: ~s~}~%" el
		    (sparql `(:select (?p ?annot) (:distinct t)
				     (,el ?p ?annot)
				     (:filter (and (not (equal ?p !owl:disjointWith))))) 
			   :use-reasoner :none :flatten t :kb kb
			   ))))))

(defun asserted-subclass-of-defined-class (kb)
  (sparql
   '(:select (?sublabel ?superlabel) (:distinct t)
     (?super !owl:equivalentClass ?other)
     (?sub !rdf:type !owl:Class)
     (?super !rdf:type !owl:Class)
     (?sub !rdfs:subClassOf ?super)
     (:optional 
      (?sub !rdfs:label ?sublabel))
     (:optional
      (?super !rdfs:label ?superlabel))
     (:filter (and
	       (not (equal ?super !material-entity))
	       (regex (str ?sub) "obi|OBI")
	       (regex (str ?super) "obi|OBI")))
     )
   :kb kb :use-reasoner :none :trace "asserted subclass of defined-class" :values nil :trace-show-query nil))

(defun covering-classes (kb)
  "Defined classes that are the union of some other set of classes"
  (let ((them
	 (sparql
	  '(:select (?me ?label) (:distinct t)
	    (?other !owl:unionOf ?list)
	    (?me !owl:equivalentClass ?other)
	    (?me !rdf:type !owl:Class)
	    (?other !rdf:type !owl:Class)
	    (:optional 
	     (?me !rdfs:label ?label))
	    (:filter (not (equal ?me !material-entity)))
	    )   :kb kb :use-reasoner :none)))
    (loop for (it name) in them collect
	 (list* it name (mapcar (lambda(e)(car (rdfs-label e kb)))  (children it kb))))))

(defun path-of-last-released-obi ()
  (let* ((merged (make-pathname :directory '(:relative "merged") :name "OBI" :type "owl"))
	 (versions (remove-if-not
		    (lambda(dir) (and
				  (#"matches" (namestring dir) ".*\\d{4}-\\d{2}-\\d{2}/")
				  (probe-file (merge-pathnames merged dir))))
		    (directory (concatenate 'string (namestring (truename "obi:releases;")) "*")
			       )))
	 (latest (car (sort versions 'string-greaterp
			    :key (lambda(el) (car (last (pathname-directory el))))))))
    (values (merge-pathnames merged latest) latest)))
    
(defun lost-terms (&key
		   (current (load-kb-jena "obi:branches;obil.owl"))
		   (previous (load-kb-jena (path-of-last-released-obi))))
  (let((kb-purls
	(sparql
	 '(:select (?thing) (:distinct t)
	   (:union
	    ((?thing ?p ?o))
	    ((?s ?thing ?o))
	    ((?s ?p ?thing)))
	   (:filter (and (isuri ?thing) (regex (str ?thing) "OBI_\\d+")))
	   )
	 :kb current :use-reasoner :none :flatten t))
       (previous-purls
	(and previous
	     (sparql
	      '(:select (?thing) (:distinct t)
		(:union
		 ((?thing ?p ?o))
		 ((?s ?thing ?o))
		 ((?s ?p ?thing)))
		(:filter (and (isuri ?thing) (regex (str ?thing) "OBI_\\d+")))
		)
	      :kb previous :use-reasoner :none :flatten t))))
    (when (set-difference (mapcar (lambda(e) (make-uri (#"replaceFirst" (uri-full e) "purl.obofoundry.org" "purl.obolibrary.org"))) previous-purls) kb-purls)
      (format t "Hmm, we seem to have lost some ids (deprecation lossage?): ~%~{~a~%~}"
	      (set-difference previous-purls kb-purls)))    
    ))

;; start checking for malformed metadata.
(defun malformed-metadata (kb)
  ;; check for multiline values for definition source, definition editor, alternative term, preferred term
  (loop for (?what ?prop ?value) in
       (sparql `(:select (?thing ?p ?value) (:distinct t)
			 (?thing ?p ?value)
			 (:filter (and
				   (or (equal ?p !definition-source)
				       (equal ?p !definition-editor)
				       (equal ?p !alternative-term)
				       (equal ?p !preferred-term))
				   (regex (str ?value) "\\n"))))
	       :use-reasoner :none :kb kb )
       do
       (format t "Multiline value for ~a(~a) on ~a(~a):~%-----~%~a~%-----~%" (rdfs-label ?prop kb) ?prop (rdfs-label ?what kb) ?what ?value)))

(defun no-definition (kb)
  (sparql '(:select (?term ?label) ()
	    (?term !rdf:type !owl:Class)
	    (?term !rdfs:label ?label)
	    (:optional (?term !'definition'@obi ?def) (?term !rdfs:label ?label))
	    (:filter (and (regex (str ?term) "OBI_") (not (regex (str ?label) "obsolete")) (not (isblank ?term)) (not (bound ?def))))
	    )
	  :kb kb
	  :use-reasoner :none
	  :trace "Terms without definitions"
	  :flatten t
	  :values nil))