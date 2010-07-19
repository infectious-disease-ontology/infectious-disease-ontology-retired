# This is a patched together conversion script, relying on a modified version of the OBI to OBO script lightly edited to work with owlapiv3

(defun ido-dont-include (uri)
  "return t to mean include this term"
  (let ((matches (or
		  (#"matches" (uri-full uri) ".*OGMS.*")
		  (#"matches" (uri-full uri) ".*IAO_.*")
		  (#"matches" (uri-full uri) "http://www.geneontology.org/formats.*"))
		  ))
    (or (and matches
	     (member uri `(,!obo:OGMS_0000063 ,!obo:OGMS_0000031 ,!obo:OGMS_0000045)))
	(not matches))))

(defun generate-ido-obo (&key (kb  (load-ontology "ido:ido-core;ido.owl"))
			 (path (translate-logical-pathname "ido:ido-core;ido.obo"))
			 (saved-by "generate-ido-obo")
			 (default-namespace "IDO")
			 (ontology-name "IDO")
			 (ontology-uri "http://purl.obolibrary.org/obo/ido.owl")
		     ) 
  (instantiate-reasoner ido :pellet-sparql)
  (setf (gethash !owl:Thing (rdfs-labels ido)) '("Thing"))
  (generate-obo :kb kb :path path :saved-by saved-by :default-namespace default-namespace
		:ontology-name ontology-name :ontology-uri ontology-uri :default-definition-source " [OBO:sourced \"IDO consortium http://www.infectiousdiseaseontology.org\"]" :filter-dont-include 'ido-dont-include :include-obsolete t))

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