; This is a patched together conversion script, relying on a modified version of the OBI to OBO script lightly edited to work with owlapiv3

(defun ido-dont-include (uri)
  "return t to mean include this term"
  (not (#"matches" (uri-full uri) "(?i).*oboinowl.*")))

  '(let ((matches (or
		  (#"matches" (uri-full uri) ".*OGMS.*")
		  (#"matches" (uri-full uri) ".*IAO_.*")
		  (#"matches" (uri-full uri) "http://www.geneontology.org/formats.*"))
		  ))
    (or (and matches
	     (member uri `(,!obo:OGMS_0000063 ,!obo:OGMS_0000031 ,!obo:OGMS_0000045)))
	(not matches))))

(defun generate-ido-obo (&key (ido  (load-ontology "ido:ido-core;ido.owl"))
			 (path (translate-logical-pathname "ido:ido-core;ido.obo"))
			 (saved-by "generate-ido-obo")
			 (default-namespace "IDO")
			 (ontology-name "IDO")
			 (ontology-uri "http://purl.obolibrary.org/obo/ido.owl")
			 ) 
  (instantiate-reasoner ido :pellet-sparql)
  (let ((comments
	 (sparql '(:select (?comment) ()
		   (?o :a !owl:Ontology)
		   (?o !rdfs:comment ?comment)
		   (:filter (regex (str ?o) ".*ido-*.owl")))
		 :use-reasoner :none :kb ido :flatten t))
	(creators 
	 (sort
	  (sparql '(:select (?creator) (:distinct t)
		    (?o :a !owl:Ontology)
		    (?o !dc:creator ?creator)
		    (:filter (regex (str ?o) ".*ido-*.owl")))
		  :use-reasoner :none :kb ido :flatten t)
	  'string-lessp 
	  :key (lambda(e) (car (last (split-at-char e #\space))))))
	(contributors 
		 (sort (sparql '(:select (?contributor) (:distinct t)
				 (?o :a !owl:Ontology)
				 (?o !dc:contributor ?contributor)
				 (:filter (regex (str ?o) ".*ido-*.owl")))
			       :use-reasoner :none :kb ido :flatten t) 
		       'string-lessp
		       :key (lambda(e) (car (last (split-at-char e #\space))))))
	(disclaimer (format nil "This file is a subset of IDO adequate for indexing using the OLS service. It does not include all logical assertions present in the OWL file, which can be obtained at ~a" ontology-uri)))
    (and contributors (setq contributors (list (format nil "Contributors: ~{~a~^, ~}" contriabutors))))
    (and creators (setq creators (list (format nil "Creators: ~{~a~^, ~}" creators))))
    (setf (gethash !owl:Thing (rdfs-labels ido)) '("Thing"))
    (generate-obo :kb ido :path path :saved-by saved-by :default-namespace default-namespace
		  :ontology-name ontology-name :ontology-uri ontology-uri :default-definition-source " [OBO:sourced \"IDO consortium http://www.infectiousdiseaseontology.org\"]" :filter-dont-include 'ido-dont-include :include-obsolete t :remarks (cons disclaimer (append comments creators contributors)))))

(defun rdfs-comments (kb)
  (let ((table (make-hash-table)))
    (maphash (lambda(uri v)
	       (declare (ignore v))
	       (setf (gethash uri table) (rdfs-comment uri kb)))
	     (v3kb-uri2entity kb))
    table))

(defvar *obi-uri-pattern* (#"compile" 'java.util.regex.pattern "^http://purl.(obolibrary|obofoundry).org/obo/OBI_\\d+$"))

(defun is-obi-uri (uri)
  (#"matches" (#"matcher" *obi-uri-pattern* (uri-full uri))))

(defun property-inverses (prop kb)
  (sparql `(:select (?p1) () (?p1 !owl:inverseOf ,prop)) :kb kb :use-reasoner :none :flatten t))

(defun property-superproperties (prop kb)
  (sparql `(:select (?p1) () (?p1 !rdfs:subPropertyOf ,prop)) :kb kb :use-reasoner :none :flatten t ))