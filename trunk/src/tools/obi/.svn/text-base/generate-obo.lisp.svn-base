(defun obo-format-time (&optional (time-value (get-universal-time)))
  "Format a time how the obo format likes to see it"
  (multiple-value-bind (second minute hour day month year dow dst zone)
      (decode-universal-time time-value)
    (declare (ignore dow dst))
    (format nil "~2,'0D:~2,'0D:~4,'0D ~2,'0D:~2,'0D"
	    day month year  hour minute
	    )))

(defun localname (uri)
  "The part of the URI after the last / or #"
  (let ((full (uri-full uri)))
    (#"replaceFirst" full ".*[/#](.*)" "$1")))

(defun ontology-of (uri)
  "The part of the URI before the last / or #"
  (let ((full (uri-full uri)))
    (let ((candidate (#"replaceFirst" full "(.*)[/#](.*)" "$1")))
      (if (equal candidate "http://purl.obolibrary.org/obo")
	  (format nil "~a/~a.owl" candidate (string-downcase (#"replaceAll" (localname uri) "_|\\d" "")))
	  (if (#"matches" candidate "http://purl.org/obo/owl/.*")
	      (format nil "~a" candidate)
	      (if (#"matches" candidate "http://www.ifomis.org/bfo/.*")
		  (#"replaceAll" candidate "(snap)|(span)$" "")
		  (if (#"matches" candidate "http://www.obofoundry.org/ro/ro.owl.*")
		      "http://www.obofoundry.org/ro/ro.owl"
		      (if (#"matches" candidate ".*OBO_REL.*")
			  "http://purl.org/obo/owl/ro_proposed"
			  (if (#"matches" candidate "http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl.*")
			      "http://purl.org/nif/ontology/BiomaterialEntities/NIF-GrossAnatomy.owl"
			      (warn "don't know ontology of ~a uri" uri)
			      )))))))))

(defun localname-namespaced (uri)
  "Abbreviate a URI using the OBO conventions for namespaces."
  (let* ((full (uri-full uri))
	 (local  (#"replaceFirst" full ".*/(.*)" "$1")))
					;    (print-db local)
					;    (setq local (#"replaceFirst" local "([A-Za-z]{2,7})_" "$1:"))
					;    (print-db local)
    (if (equal full "http://www.ifomis.org/bfo/1.1#Entity")
	"BFO:Entity"
	(if (is-obi-uri uri)
	    (#"replaceFirst" local "([A-Za-z]{2,7})_" "$1:")
	    (if (#"matches" local "^OBO_REL.*")
		(#"replaceAll" local "OBO_REL#" "ROP:")
		(if (#"matches" local "^ro\\.owl#.*")
					; Relation ontology
		    (progn		;(print-db local)
		      (concatenate 'string "RO:" (#"replaceFirst" local ".*#(.*)" "$1")))
		    (if (#"matches" local "^NIF-GrossAnatomy.owl.*")
			(concatenate 'string "birn_anat:" (#"replaceFirst" local ".*#(.*)" "$1"))
			(if (#"matches" local "^MGEDOntology\\.owl.*")
					; MGED
			    (concatenate 'string "MGED:" (#"replaceFirst" local ".*#(.*)" "$1"))
			    (if (#"matches" local "[A-Z]+.owl#msi_.*")
					; the MSI ontologies. These shouldn't actually be here, having been replaced by OBI ids before this stage.
					; While debugging warn about them and give them a fake id.
				(progn
					;(format *error-output* "Found (and replaced) MSI id ~a~%" local)
				  (#"replaceFirst" local "[A-Z]+.owl#msi_(.*)" "OBI:X$1"))
				(let ((clean (#"replaceFirst" (#"replaceFirst" local "([A-Za-z]{2,7})_" "$1:") "#" ":")))
		      			  
				  (#"replaceFirst" clean "(\\S+):\\S+?:(\\d+)" "$1:$2")))
			    ))))))))
	
(defvar *all-prefixes* nil)

(defun definition-source-dbxref (class kb &key (relative-to "OBI")
				       (default-definition-source " [OBI:sourced \"OBI Consortium http://purl.obolibrary.org/obo/obi\"]"))
  "Generate the dbxref for a definition. Involves various munging to map OBI's more broad range of definition sources. Return empty list since definitions need such even if there are no references"
  (let ((definition-source (sparql `(:select (?source) (:distinct t)
					     (,class !definition-source ?source))
				   :use-reasoner :none :kb kb :flatten t)))
    (if definition-source	     ; note there may be more than one
	(values
	 (with-output-to-string (s)
	   (format s " [~{~a~^, ~}]" ;; dbxref entries are comma separated
		   (loop for defs in definition-source
		      for is-msi = (or (search "MSI.owl" defs) (search "NMR.owl" defs) (search "CHROM.owl" defs)) ; special case the MSI ontologies. Prototype.
		      collect 
		      (if is-msi
			  (format nil "MSI:~a ~s" (caar (all-matches defs ".owl#msi_(.*)" 1)) defs) ; reformat as MSI:xxxxxx, as they all share the same id prefix in the source.
			  ;; not msi. See if we have a term = foo:xxxxx. Allow ones that are in the xrf file to pass. 
			  (let ((match (car (all-matches defs "(?i)^(TERM:\\s*){0,1}(\\S+):\\s*(\\S+?)(\\s*#\\s*(.*)){0,1}$" 2 3 5))))
			    (if (and match (not (equalp (car match) "SEP"))) ; For now sep: ids are not registed in the GO
				(let ((prefix (car match)) (suffix (second match)) (comment (third match))) ; we've matched
				  (setq prefix (string-upcase prefix)) ; normalize to upcase prefix
				  (pushnew prefix *all-prefixes* :test 'equalp) ; for debugging: Keep track of which prefixes we've used
				  (when (equal prefix "PMID") (setq prefix "PUBMED")) ; you say PUBMED I say PMID
				  (format nil "~a:~a ~a" prefix suffix
					  (if (member comment '("" nil) :test 'equal)
					      ""
					      (format nil "\"~a\"" comment))))
				;; check for embedded ISBNs (should be fixed in source)
				(let ((has-isbn (caar (all-matches defs "ISBN-\\d+:\\s+([0-9-]*)" 1))))
				  (if has-isbn
				      (format nil "ISBN:~a ~s" has-isbn defs)
				      ;; otherwise, we are not a term and use OBO:sourced and the string for definition source
				      (format nil "OBO:sourced ~s" (#"replaceAll" (#"replaceFirst" defs "OBI_" "OBI:" ) "\\n" " "))))))))))
	 (loop for defs in definition-source
	    for is-msi = (or (search "MSI.owl" defs) (search "NMR.owl" defs) (search "CHROM.owl" defs)) ; special case the MSI ontologies. Prototype.
	    when is-msi
	    collect (format nil "xref_analog: MSI:~a ~s~%" (caar (all-matches defs ".owl#msi_(.*)" 1)) defs))
	 )
	;; no definition source. Return empty list
	(if (and default-definition-source (#"matches" (uri-full class)  (format nil ".*/~a_.*" relative-to)))
	    default-definition-source
	    (progn 
	      (format nil " [OBO:imported ~s]" (ontology-of class))
	      )))))
		     

(defun quote-for-obo (string)
  (#"replaceAll" 
  (#"replaceAll" 
   (#"replaceAll" (#"replaceAll" string "\\\\n" "\\n") "([,\\\\])" "\\\\$1" ) "\\n" "\\\\n")
   "\"" "\\\""))
  
(defun generate-obo ( &key (kb (load-kb-jena "obi:branches;obil.owl"))
		     (classes-matching ".*([A-Za-z]+)_\\d+") ; ignored atm
		     (properties-matching ".*OBI_\\d+") ; ignored atm
		     (path "obi:branches;obi.obo")
		     (saved-by "obi")
		     (default-namespace "OBI")
		     (ontology-name "OBI")
		     (default-definition-source " [OBI:sourced \"OBI Consortium http://purl.obolibrary.org/obo/obi\"]")
		     (ontology-uri "http://purl.obolibrary.org/obo/obi.owl")
		     ) 
  (with-open-file (f  path :direction :output :if-does-not-exist :create :if-exists :supersede)
    (let ((*current-labels* (rdfs-labels kb)) ; speed optimization for labels and comments
          (*current-comments* (rdfs-comments kb)))
      (macrolet ((rdfs-label (e) `(gethash ,e *current-labels*)) 
		 (rdfs-comment (e) `(gethash ,e *current-comments*)))
					; write canned header
	(format f "format-version: 1.2
date: ~a
saved-by: ~a
auto-generated-by: http://purl.obolibrary.org/obo/obi/repository/trunk/src/tools/build/generate-obo.lisp
default-namespace: ~a
idspace: RO http://www.obofoundry.org/ro/ro.owl# \"OBO Relation ontology official home on OBO Foundry\"
idspace: ROP http://purl.org/obo/owl/OBO_REL# \"Relation ontology proposed terms\"
idspace: PATO http://purl.org/obo/owl/PATO# \"Phenotype Ontology\"
idspace: snap http://www.ifomis.org/bfo/1.1/snap# \"BFO SNAP ontology (continuants)\"
idspace: span http://www.ifomis.org/bfo/1.1/span# \"BFO SPAN ontology (occurrents)\"
idspace: OBI http://purl.obolibrary.org/obo/OBI_ \"Ontology for Biomedical Investigations\"
idspace: CHEBI http://purl.org/obo/owl/CHEBI# \"Chemical Entities of Biological Interest\"
idspace: CL http://purl.org/obo/owl/CL# \"Cell Ontology\"
idspace: NCBITaxon http://purl.org/obo/owl/NCBITaxon# \"NCBI Taxonomy\"
idspace: birn_anat http://ontology.neuinfo.org/NIF/BiomaterialEntities/NIF-GrossAnatomy.owl# \"NIF Standard Anatomy\"
idspace: IAO http://purl.obolibrary.org/obo/IAO_ \"Information Artifact Ontology\"
idspace: BFO http://www.ifomis.org/bfo/1.1/ \"Basic Formal Ontology\"
idspace: GO http://purl.org/obo/owl/GO# \"Gene Ontology\"
idspace: FMA http://purl.org/obo/owl/FMA# \"Foundational Model of Anatomy\"
idspace: PRO http://purl.org/obo/owl/PRO# \"Protein Ontology\"
idspace: UO http://purl.org/obo/owl/UO# \"Unit Ontology\"
idspace: ENVO http://purl.org/obo/owl/ENVO# \"Environment Ontology\"
idspace: VO http://purl.obolibrary.org/obo/VO \"Vaccine Ontology\"
idspace: SO http://purl.org/obo/owl/SO# \"Sequence Ontology\"
idspace: HPO http://purl.org/obo/owl/HP# \"Human Phenotype Ontology\"
remark: This file is a subset of ~a adequate for indexing using the OLS service. It does not include all logical assertions present in the OWL file, which can be obtained at ~a

" saved-by default-namespace
		(obo-format-time) ontology-name ontology-uri)
	(loop for class in (set-difference (descendants !owl:Thing kb) (list !protegeowl:DIRECTED-BINARY-RELATION !protegeowl:PAL-CONSTRAINT))
	   for obsolete = (and (not (eq class !<http://www.geneontology.org/formats/oboInOwl#ObsoleteClass>))
			       (or 
				(member !obsolete-class (ancestors class kb))
				(or (#"matches" (or (rdfs-label class) "") "^obsol.*"))
				(or (#"matches" (or (rdfs-comment class) "") "(?i)^obsol.*"))
				(or (#"matches" (or (rdfs-label class) "") "^_.*"))
				))
	   unless obsolete ;(#"matches" (uri-full class) classes-matching)
	   do 
	   ;; write term ID and name
	   (format f "[Term]~%id: ~a~%name: ~a~%"
		   ;;(#"replaceFirst"  (#"replaceFirst" (uri-full class) ".*(OBI|CHEBI|CL|NCBITaxon)(_\\d+)" "$1:$1$2") "OBI:" "")
		   (localname-namespaced class)
		   (or (rdfs-label class) (localname class)))
	   ;; write definition and dbxref for them
	   (let ((comment (rdfs-comment class)))
	     (unless (or (null comment) (equal comment ""))
	       (multiple-value-bind (dbxrefs analogs) (definition-source-dbxref class kb :relative-to ontology-name :default-definition-source default-definition-source)
		 (format f "def:\"~a\"~a~%"  (quote-for-obo comment) dbxrefs)
		 (loop for xra in analogs do (write-string xra f))
		 )))
	   ;; put obsolete marked for obsolete classes
	   (if obsolete
	       (format f "is_obsolete: true~%")
	       (loop for super in (parents class kb)
		    unless (#"matches" (rdfs-label super) "^_.*")
		  do (format f "is_a: ~a ! ~a~%"  (localname-namespaced super) (rdfs-label super))))
	   (terpri f))
	;; now write out the list of properties we will use (we don't actually write their values out yet in the above)
	(loop for proptype in (list !owl:AnnotationProperty !owl:DatatypeProperty !owl:ObjectProperty )
	   do
	   (loop for prop in (sparql `(:select (?prop) (:distinct t) (?prop !rdf:type ,proptype))  :use-reasoner :jena :kb kb :flatten t)
	      for name = (localname-namespaced prop)
		for supers = (mapcar 'aterm-to-sexp
			       (apply 'append (mapcar 'set-to-list 
						      (set-to-list
						       (#"getSuperProperties" (kb-kb kb) (get-entity prop kb))))))
		
	      for obsolete = (and (not (eq prop !<http://www.geneontology.org/formats/oboInOwl#ObsoleteProperty>))
				  (or (member !oboinowl:ObsoleteProperty supers)
				      (or (#"matches" (or (rdfs-label prop) "") "^obsol.*"))
				      (or (#"matches" (or (rdfs-comment prop) "") "(?i)^obsol.*"))
				      ))
	      unless obsolete ;(#"matches" (uri-full prop) properties-matching)
	      do
	      (unless (or (#"matches" name ".*[A-Z]{4,}.*") ; protege noise is all upper case
			  (#"matches" name "^protege.*")
			  (#"matches" (uri-full prop) "(http://purl.org/dc/.*)|(.*oboInOwl.*)")
			  (member name '("OBO_REL:relationship") :test 'equal)
			  )
		(format f "[Typedef]~%id: ~a~%name: ~a~%"
			(localname-namespaced prop)
			(or (rdfs-label prop) (localname prop)))
		(let ((comment (rdfs-comment prop)))
		  (unless (or (null comment) (equal comment ""))
		    (if (#"matches" comment "(?s).*beta.*") (print comment))
		    (multiple-value-bind (xrefs analogs) (definition-source-dbxref prop kb)
		      (format f "def:\"~a\"~a~%" (quote-for-obo comment) xrefs)
		      (loop for xra in analogs do (write-string xra f))
		      )))
		;; compute and write inverse relation
		(let ((inverses
		       (mapcar 'aterm-to-sexp
			       (set-to-list
				(#"getInverses" (kb-kb kb) (get-entity prop kb)) ))))
		  (loop for inverse in inverses do
		       (format f "inverse_of: ~a ! ~a~%" (localname-namespaced inverse) (rdfs-label inverse))))
		  (loop for super in supers 
		     unless (eq super !ro:relationship)
		     do (format f "is_a: ~a ! ~a~%" (localname-namespaced super) (or (rdfs-label super) (localname super))))
		(terpri f))
	      ))))))

(defun generate-iao (&key (kb (load-kb-jena :iaol))
		     (path "~/Desktop/iao.obo")
		     (saved-by "iao")
		     (default-namespace "IAO")
		     (ontology-name "IAO")
		     (ontology-uri "http://purl.obolibrary.org/obo/iao.owl")
		     ) 
  (generate-obo :kb kb :path path :saved-by saved-by :default-namespace default-namespace
		:ontology-name ontology-name :ontology-uri ontology-uri :default-definition-source nil))
    

#|		      
  <owl:Class rdf:about="http://purl.obolibrary.org/obo/OBI_0400082"><!-- photodetector -->
    <IAO_0000117 xml:lang="en">John Quinn</IAO_0000117> <-- definition editor
    <IAO_0000112 xml:lang="en">A photomultiplier tube, a photo diode</IAO_0000112> <- example of usage
    <IAO_0000115 xml:lang="en">A photodetector is a device used to detect and measure the intensity of radiant energy through photoelectric action. In a cytometer, photodetectors measure either the number of photons of laser light scattered on inpact with a cell (for example), or the flourescence emmited by excitation of a lourescent dye.</IAO_0000115>
    <rdfs:label xml:lang="en">photodetector</rdfs:label>
    <rdfs:subClassOf>
      <owl:Class rdf:about="http://purl.obolibrary.org/obo/OBI_0400003"/><!-- instrument -->
    </rdfs:subClassOf>
    <rdfs:subClassOf>
      <owl:Class rdf:about="http://purl.obolibrary.org/obo/OBI_0400002"/><!-- device -->
    </rdfs:subClassOf>
    <IAO_0000114 rdf:resource="http://purl.obolibrary.org/obo/IAO_0000123"/> <-- curation status
    <IAO_0000111 xml:lang="en">photodetector</IAO_0000111> <- synonym
    <rdfs:subClassOf>
      <owl:Restriction>
        <owl:onProperty>
          <owl:ObjectProperty rdf:about="http://purl.obolibrary.org/obo/OBI_0000306"/><!-- has_function -->
        </owl:onProperty>
  p      <owl:someValuesFrom>
          <owl:Class rdf:about="http://purl.obolibrary.org/obo/OBI_0000382"/><!-- measure function -->
        </owl:someValuesFrom>
      </owl:Restriction>
    </rdfs:subClassOf>
    <IAO_0000119 xml:lang="en">http://einstein.stanford.edu/content/glossary/glossary.html</IAO_0000119> <- definition source
  </owl:Class>
|#