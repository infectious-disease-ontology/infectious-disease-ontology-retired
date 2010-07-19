; (add-directory-jars-to-class-path "/Users/alanr/Downloads/2009-08-06/poi-3.2-FINAL" t)
;; (load "~/lsw/biopax/proto/obo.lisp")

(defvar *ido-term-to-uri* (make-hash-table :test 'equalp))

(defun ido-to-owl (&key (file "ido:ido-core;IDO-editors-worksheet.xls"))
  (let* ((sheets (list-sheets :file file))
	 (termdefs (loop for sheet in sheets
		      when (member (car sheet) 
				   '("Role" "Disposition" "Quality" "Process" "GDC" "Material Entity"	 
				     "Temporal Interval" "Defined Class"
				     "Population_DC" "Pathogen_DC" "Host_DC" "Infection_DC" "Treatment_DC" 

				     )
				   :test 'equal)
		      append (get-ido-sheet (second sheet) (first sheet)))))
    (setq @ termdefs)
    (setq @@ sheets)
    (make-ido-term-to-uri termdefs)
;;    (print-db (check-isas termdefs))
    ;;(check-historical termdefs)
    (make-ido-owl termdefs)
    ))

(defun get-ido-sheet (sheet sheet-name)
  (destructuring-bind (headers . rows)
      (loop for rowno from (#"getFirstRowNum" sheet) to (#"getLastRowNum" sheet) 
	 with nocells = (loop for row below (#"getPhysicalNumberOfRows" sheet)
			   maximize (or (and (not (#"getRow" sheet row)) 0)
					(#"getPhysicalNumberOfCells" (#"getRow" sheet row))))
	 for row = (#"getRow" sheet rowno)
	 for potential = (and row 
			      (list sheet-name rowno
				    (loop for colno below nocells
				       for cell = (#"getCell" row colno)
				       collect (and cell (#"toString" cell)))))
	 for thereyet = (or thereyet (member "Term" (third potential) :test 'equalp))
	 when (and thereyet row) collect potential)
    (let ((headerkeys (mapcar (lambda(s)(intern (substitute #\- #\space (string-upcase s))'keyword)) (third headers))))
      (loop for (sheet rowno row) in rows
	 collect
	   (append `((:sheet ,sheet) (:row ,(1+ rowno)))
		   (loop for key in headerkeys
		      for cell in row
		      if (and (equal key :synonym) cell)
		      do (if (equal cell "")
			       (setq cell nil)
			       (setq cell (mapcar (lambda(e) (string-trim " " e)) (split-at-regex cell "[;,]"))))
		      collect (list key cell)))))))

(defun check-isas (termdefs)
  (let ((terms 
	 (loop for entry in termdefs
	    for term = (second (assoc :term entry))
	    for isa = (second (assoc :is_a entry))
	    for inheresin = (second (assoc :inheres_in entry))
	    if (and term (find :is_a entry :key 'car)
		      (null isa))
	    do (warn "Missing is-a ~a" entry)
	    else when (and entry term isa (not (equal term "")))
	    collect (list term isa)
;	    when (and inheresin term (not (equal term "")))
;	    collect (list term inheresin )
	      )))
    (remove-duplicates (set-difference
			(set-difference (mapcar 'second terms) (mapcar 'first terms) :test 'equalp)
			(mapcar 'car *ido-external-terms*) :test 'equalp)
			:test 'equalp)))

(defun make-ido-term-to-uri (termdefs)
  (let ((table (make-hash-table :test 'equalp)))
    (loop for entry in termdefs
       for term = (second (assoc :term entry))
       for newid = (second (assoc :new-id entry))
       when (and term (not (equal term "")))
       do
;	 (print-db term newid (assoc :is_a entry))
       (if (not newid)
	   (warn "Missing id for term ~a" term )
	   (setf (gethash term table)
		 (make-uri nil (format nil "obo:IDO_~7,'0d" (round (read-from-string newid))))))

       )
;    (setf (gethash "infectious disorder" table) !obo:OGMS_0000048)
    (setq *ido-term-to-uri* table)
    ))

(defun make-ido-owl (termdefs)
  (with-ontology ido (:about "http://purl.obolibrary.org/obo/ido.owl" :base "http://purl.obolibrary.org/obo/ido.owl" :collecting t)
      ((asq
	(imports !<http://www.ifomis.org/bfo/1.1>)
	(imports !<http://purl.obolibrary.org/obo/iao/dev/ontology-metadata.owl>)
	(imports !<http://purl.obolibrary.org/obo/ogms.owl>)
	(imports !<http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl>)
	(annotation-assertion  !rdfs:comment !obo:ido.owl "The core Infectious Disease Ontology is an ontology of entities generally relevant to both the biomedical and clinical aspects of infectious diseases, such as 'pathogen', 'host', 'vector', and 'vaccine'.  The structure of IDO adheres to the Basic Formal Ontology.  Terms in IDO that are within the scope of other OBO Foundry ontologies, such as the Gene Ontology, are derived from those ontologies.  Other terms are defined as much as possible as cross-products of terms from Foundry ontologies. For more information, see http://www.infectiousdiseaseontology.org/Home.html@en")
	(annotation-assertion !rdfs:comment !obo:ido.owl "This ontology is in early development. Expect it to change.@en")
	(annotation-assertion !rdfs:seeAlso !obo:ido.owl !<http://www.infectiousdiseaseontology.org/Home.html>)
	(annotation-assertion !dc:creator !obo:ido.owl "Lindsay Cowell")
	(annotation-assertion !dc:creator !obo:ido.owl "Albert Goldfain")
	(annotation-assertion !dc:creator !obo:ido.owl "Alexander Diehl")
	(annotation-assertion !dc:contributor !obo:ido.owl "Barry Smith")
	(annotation-assertion !dc:contributor !obo:ido.owl "Alan Ruttenberg")
	(annotation-assertion !<http://protege.stanford.edu/plugins/owl/protege#defaultLanguage> !obo:ido.owl "en")
	(declaration (annotation-property !obo:IAO_0000115))
	(declaration (annotation-property !obo:IAO_0000117))
	(declaration (annotation-property !obo:IAO_0000119))
	(declaration (annotation-property !obo:IAO_0000116))
	(declaration (annotation-property !obo:IAO_0000118))
	(declaration (annotation-property !obo:OBI_0000283))
	(declaration (annotation-property !<http://protege.stanford.edu/plugins/owl/protege#defaultLanguage>))
	(declaration (object-property !oborel:part_of))
	(annotation-assertion !dc:date !obo:ido.owl (:literal "2010-05-19" !xsd:date))
;	(declaration (class  !obo:IDO_9999999))
;	(annotation-assertion !rdfs:label !obo:IDO_9999999 "_defined class")
;	(annotation-assertion !obo:IAO_0000116 !obo:IDO_9999999 "placeholder"))
;       (loop for (term id) in *ido-defined-classes*
;	  do
;	  (as `(declaration (class ,id)))
;	  (as `(annotation-assertion !rdfs:label ,id ,(format nil "_defined ~a@en" term)))
;	  (as `(annotation-assertion !obo:IAO_0000116 ,id  "placeholder"))
;	    (as `(subclass-of ,(or (gethash term *ido-term-to-uri*)
;					    (second (assoc term *ido-external-terms* :test 'equalp))
;					    (second (assoc term *ido-defined-classes* :test 'equalp)))
;			       ,id))
;	  (as `(subclass-of ,id !obo:IDO_9999999))
	  )
       (asq
	(declaration (class !<http://purl.org/obo/owl/GO#GO_0005575> ))
	(subclass-of !<http://purl.org/obo/owl/GO#GO_0005575> !snap:MaterialEntity)
	(annotation-assertion !rdfs:label !<http://purl.org/obo/owl/GO#GO_0005575> "cellular component")
	(declaration (class !<http://purl.org/obo/owl/GO#GO_0005615> ))
	(subclass-of !<http://purl.org/obo/owl/GO#GO_0005615> !snap:MaterialEntity)
	(subclass-of !<http://purl.org/obo/owl/CARO#CARO_0000000> !snap:MaterialEntity)
	(annotation-assertion !rdfs:label !<http://purl.org/obo/owl/CARO#CARO_0000000> "anatomical entity")
	(subclass-of !<http://purl.org/obo/owl/CHEBI#CHEBI_23367> !snap:MaterialEntity)
	(annotation-assertion !rdfs:label !<http://purl.org/obo/owl/CHEBI#CHEBI_23367> "molecular entity")
	(annotation-assertion !rdfs:label !<http://purl.org/obo/owl/GO#GO_0005615> "extracellular space")
	(subclass-of !<http://purl.org/obo/owl/GO#GO_0005615> (object-some-values-from !oborel:part_of !obo:OBI_0100026))
	(declaration (class !obo:OBI_0100026))
	(subclass-of !obo:OBI_0100026 !snap:MaterialEntity)
	(subclass-of !<http://purl.org/obo/owl/NCBITaxon#NCBITaxon_10239> !obo:OBI_0100026 )
	(annotation-assertion !rdfs:label !<http://purl.org/obo/owl/NCBITaxon#NCBITaxon_10239> "Viruses")
	(annotation-assertion !rdfs:label !obo:OBI_0100026 "organism@en")
	(annotation-assertion !<http://purl.obolibrary.org/obo/OBI_0000283> !obo:OBI_0100026 !<http://purl.obolibrary.org/obo/obi.owl>)
	(annotation-assertion !obo:IAO_0000115 !obo:OBI_0100026 "An organism is material entity that is an individual living system, such as animal, plant, bacteria or virus, that is capable of replicating or reproducing, growth and maintenance in the right environment. An organism may be unicellular or made up, like humans, of many billions of cells divided into specialized tissues and organs."))

       (loop for (id (term) definition) in (historical-ido-terms)
	    for uri = (make-uri (#"replaceAll" id "IDO:" "http://purl.obolibrary.org/obo/IDO_"))
	    do
	    (as
	     `(declaration (class ,uri))
	     `(subclass-of ,uri !oboinowl:ObsoleteClass)
	     `(annotation-assertion !rdfs:label ,uri ,(format nil "_obsolete_~a@en" term)))
	    (when (and definition (not (equal definition "")))
	      (as
	       `(annotation-assertion !obo:IAO_0000115 ,uri 
				      ,(format nil "'~a'-~a: ~a@en" definition
					       (#"replaceAll" id "IDO:" "ID[O]:")
					       definition))
	       `(annotation-assertion !obo:IAO_0000116 ,uri ,(format nil "id '~a'-~a from IDO 2007. May have replacement - TBD@en" term
							   (#"replaceAll" id "IDO:" "ID[O]:")
							   id) )))
	    )
       (loop for entry in termdefs
	  for term = (second (assoc :term entry))
	  for uri = (gethash term *ido-term-to-uri*)
	  for definedclass? = (equal (second (assoc :sheet entry)) "Defined Classes")
	  for id = (second (assoc :id entry))
	  for hasid? = (and id (not (equal id "")) (search "IDO:" id))
	  for deprecated = (#"matches" (or (second (assoc :is_a  entry)) "")
				       "^((obsolete)|(deprecate)|(deprecated)).*")
	  for dont = (or (not (second (assoc :is_a entry)))
			 (#"matches" (second (assoc :is_a  entry)) "^((pending)|(merge to)).*"))
	  for isa-uri =
	  (cond (definedclass?
		 (second (assoc (second (assoc :is_a entry)) *ido-defined-classes* :test 'equalp)))
		(t (or (gethash (second (assoc :is_a entry)) *ido-term-to-uri*)
		       (second (assoc (second (assoc :is_a entry))
				      *ido-external-terms* :test 'equalp)))))
	  for definition = (second (assoc :natural-language-definition entry))
	  for formal-definition = (second (assoc :formal-definition entry))
	  for comment = (second (assoc :comments entry))
	  for is_a = (second (assoc :is_a entry))
	  for synonyms = (second (assoc :synonym entry))
	    do (print-db term uri definedclass? id hasid? isa-uri definition is_a synonyms deprecated dont)
	  when (and term (not (equal term "")) (not dont))
	  do
	    (print-db term uri definedclass? id hasid? isa-uri definition is_a synonyms deprecated)
	    (if (and (not definedclass?) (not (and uri isa-uri)) (not deprecated))
		(progn (warn "Whoops - need uri (~a) and isa-url (~s) - ~a" uri isa-uri entry) nil)
		(progn
		  (as
		   `(declaration (class , uri))
		   `(subclass-of ,uri ,(if (or deprecated(#"matches" term "^_{0,1}obsolete_.*")) !oboinowl:ObsoleteClass isa-uri))
		   `(annotation-assertion !rdfs:label ,uri ,(format nil "~a~a@en" (if deprecated "_obsolete_" "") term))
		   `(annotation-assertion !obo:IAO_0000117 ,uri "Lindsay Cowell")
		   `(annotation-assertion !obo:IAO_0000117 ,uri "Albert Goldfain")
		   `(annotation-assertion !obo:IAO_0000117 ,uri "Alexander Diehl"))
		  (when definition
		    (as `(annotation-assertion !obo:IAO_0000115 ,uri ,(format nil "~a@en"  definition))))
		  (when (and formal-definition (not (#"matches" formal-definition "\\s*")))
		    (as `(annotation-assertion !obo:IAO_0000116 ,uri ,(format nil "Formal definition: ~a@en" formal-definition))))
		  (when (and comment (not (#"matches" comment "\\s*")))
		    (as `(annotation-assertion !obo:IAO_0000116 ,uri ,(format nil "~a@en" comment))))
		  (loop for syn in synonyms do
		       (as `(annotation-assertion !obo:IAO_0000118 ,uri ,(format nil "~a@en" syn))))
		  (and hasid?
		       (let ((uri (make-uri (#"replaceAll" id "IDO:" "http://purl.obolibrary.org/obo/IDO_"))))
			
			 (as `(declaration (class ,uri))
			     `(subclass-of ,uri !oboinowl:ObsoleteClass)
			     `(annotation-assertion !rdfs:label ,uri ,(format nil "_obsolete_~a@en" term)))
			 (when definition
			   (as `(annotation-assertion !obo:IAO_0000115 ,uri ,(format nil "'~a'-~a: ~a~en" term id definition))))
			 (as `(annotation-assertion !obo:IAO_0000116 ,uri ,(format nil "id for '~a'-~a replaced by ~a (ID collision)@en" term id (uri-full uri))))))
		  ))
	       )
	    )
       ido
       ))

(defvar *historical-ido* nil)

(defun check-historical (termdefs)
  (let ((historical (historical-ido-terms)))
    (map nil 'print
	 (loop for entry in termdefs
	    for term = (second (assoc :term entry))
	    for id = (second (assoc :id entry))
	    when (and id entry (not (equal term "")) (not (equal id "")) (second (assoc id historical :test 'equal)))
	    collect (list id term (second (assoc id historical :test 'equal)))))
    (map nil 'princ (mapcar 'cdr (sort 
		     (loop for (id name) in historical
			for in-sheet = (find-if (lambda(e) (or (equal name (second (assoc :term e)))
							       (member name (second (assoc :synonym e)) :test 'equal)))
						termdefs)
			if in-sheet 
			collect (cons name
				      (format nil "historical ~a : ~s -> ~a - ~a row ~a~%" id name (or (second (assoc :id in-sheet)) "no id")
					      (second (assoc :sheet in-sheet))
					      (second (assoc :row in-sheet))))
			else collect (cons name
					   (format nil "historical ~a : ~s - no mapping, deprecate?~%" id name))
			) 'string-lessp :key 'car)))
    ))

(defun historical-ido-terms (&optional id-is-uri)
  (or *historical-ido*
      (setq *historical-ido*
	    (let ((kb (load-ontology "ido:ido-core;historical;IDO-1-3-oboconv.owl")))
	      (let ((*current-labels* (rdfs-labels kb)))
		(mapcar (lambda(e)
			  (list (if id-is-uri
				    (first e)
				    (#"replaceAll" (#"replaceAll" (uri-full (first e)) ".*#" "") "_" ":"))
				(second e) (third e)))
			(loop for class in
			     (remove-if-not (lambda(e) (search "IDO#" (uri-full e)))
					    (descendants !owl:Thing kb))
			     collect (list class
					   (gethash class *current-labels*)
					   (rdfs-comment class kb)
					   ))))))))

(defun ido-2008-terms ()
  (let ((obo (make-instance 'obo :path "ido:ido-core;historical;IDO-2009-05-15.obo")))
    (read-obo obo)
    (loop for record in (terms obo) 
       for id = (getf (cdr record) :id)
       for name = (getf (cdr record) :name)
       when (search "IDO:" id)
       collect (list id name))))

(defparameter *ido-external-terms*
  (eval-uri-reader-macro
   '(("object aggregate"  !<http://www.ifomis.org/bfo/1.1/snap#ObjectAggregate>)
     ("process" !span:Process)
     ("processual entity" !span:ProcessualEntity)
     ("disease course(OGMS:0000063)" !obo:OGMS_0000063)
     ("disease course" !obo:OGMS_0000063)
     ("material entity that is alive" !obo:OBI_0100026 !obo:obi.owl) ; organism
     ("material entity" !<http://www.ifomis.org/bfo/1.1/snap#MaterialEntity>)
     ("organism"   !obo:OBI_0100026 !obo:obi.owl)
     ("quality"  !<http://www.ifomis.org/bfo/1.1/snap#Quality>)
     ("disposition"  !<http://www.ifomis.org/bfo/1.1/snap#Disposition>)
     ("occurrent"  !<http://www.ifomis.org/bfo/1.1/span#Occurrent>)
     ("temporal interval"  !<http://www.ifomis.org/bfo/1.1/span#TemporalInterval>)
     ("generically dependent continuant"  !<http://www.ifomis.org/bfo/1.1/snap#GenericallyDependentContinuant>)
     ("object"  !<http://www.ifomis.org/bfo/1.1/snap#Object>)
     ("disease" !obo:OGMS_0000031)
     ("disorder" !obo:OGMS_0000045)
     ("quality?" !<http://www.ifomis.org/bfo/1.1/snap#Quality>)
     ("site?" !<http://www.ifomis.org/bfo/1.1/snap#Site>)
     ("site" !<http://www.ifomis.org/bfo/1.1/snap#Site>)
     ("colonized quality?" !obo:IDO_0000459)
     ("infected quality?" !obo:IDO_0000460)
     ("role" !<http://www.ifomis.org/bfo/1.1/snap#Role>)
     ("cellular component" !<http://purl.org/obo/owl/GO#GO_0005575> !oboont:GO)
     ("extracellular space" !<http://purl.org/obo/owl/GO#GO_0005615>  !oboont:GO)
     ("anatomical entity" !<http://purl.org/obo/owl/CARO#CARO_0000000>)
     ("molecular entity" !<http://purl.org/obo/owl/CHEBI#CHEBI_23367>)
     ("virus" !<http://purl.org/obo/owl/NCBITaxon#NCBITaxon_10239>)
     )))

(defparameter *ido-defined-classes*
  (eval-uri-reader-macro '(("organism" !obo:IDO_9099998)
    ("material entity" !obo:IDO_9099997)
    ("cellular component" !obo:IDO_9099996)
    ("infection" !obo:IDO_9099995)
    ("infectious disease" !obo:IDO_9099994)
    ("quality"  !obo:IDO_9099993)
    ("role" !obo:IDO_9099992)
    ("disposition" !obo:IDO_9099991)
			   )))

(defparameter *ido-noise-classes* 
  (append *obo-noise-classes* (list !obo:IDO_9999999 !obi:OBI_0000449 !obi:OBI_0000233 !obi:OBI_0000683 !obi:OBI_0600065 !snap:Object !snap:FiatObjectPart !snap:SpatialRegion !span:ProcessBoundary !span:ProcessualContext !span:FiatProcessPart !span:ProcessAggregate !span:SpatiotemporalRegion !owl:Nothing !snap:ObjectBoundary !snap:Site )
	  (mapcar 'second *ido-defined-classes*)))