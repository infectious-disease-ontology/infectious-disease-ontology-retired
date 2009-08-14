; (add-directory-jars-to-class-path "/Users/alanr/Downloads/2009-08-06/poi-3.2-FINAL" t)
;; (load "~/lsw/biopax/proto/obo.lisp")

(defvar *ido-term-to-uri* (make-hash-table :test 'equalp))

(defun ido-to-owl (&key (file "ido:ido-core;IDO-editors-worksheet.xls"))
  (let* ((xls (new 'hssf (namestring (truename file))))
	 (workbook (get-java-field xls "hssfworkbook" t))
	 (sheets (loop for n below (#"getNumberOfSheets" workbook)
		    collect (list (#"getSheetName" workbook n) (#"getSheetAt" workbook n))))
	 (termdefs (loop for sheet in sheets
		      when (member (car sheet) 
				   '("Roles" "Dispositions" "Qualities" "Processes" "Objects"	 
				     "Object Aggregates" "Temporal Intervals" "Sites" "Defined Classes")
				   :test 'equal)
		      append (get-sheet (second sheet) (first sheet)))))
    (setq @ termdefs)
    (setq @@ sheets)
    (make-ido-term-to-uri termdefs)
    (print-db (check-isas termdefs))
    ;;(check-historical termdefs)
    (make-ido-owl termdefs)
    ))

(defun get-sheet (sheet sheet-name)
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
       (if (not newid)
	   (warn "Missing id for term ~a" term )
	   (setf (gethash term table)
		 (make-uri nil (format nil "obo:IDO_~7,'0d" (round (read-from-string newid))))))

       )
    (setf (gethash "infectious disorder" table) !obo:OGMS_0000048)
    (setq *ido-term-to-uri* table)
    ))

(defun make-ido-owl (termdefs)
  (with-ontology ido (:about "http://purl.obolibrary.org/obo/ido.owl" :base "http://purl.obolibrary.org/obo/ido.owl")
      ((owl-imports !<http://www.ifomis.org/bfo/1.1>)
       (owl-imports !<http://purl.obolibrary.org/obo/iao/dev/ontology-metadata.owl>)
       (owl-imports !<http://purl.obolibrary.org/obo/ogms.owl>)
       (owl-imports !<http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl>)
       (ontology-annotation !rdfs:comment (literal "The core Infectious Disease Ontology is an ontology of entities generally relevant to both the biomedical and clinical aspects of infectious diseases, such as 'pathogen', 'host', 'vector', and 'vaccine'.  The structure of IDO adheres to the Basic Formal Ontology.  Terms in IDO that are within the scope of other OBO Foundry ontologies, such as the Gene Ontology, are derived from those ontologies.  Other terms are defined as much as possible as cross-products of terms from Foundry ontologies. For more information, see http://www.infectiousdiseaseontology.org/Home.html" :|@en|))
       (ontology-annotation !rdfs:comment (literal "This ontology is in early development. Expect it to change." :|@en|))
       (ontology-annotation !rdfs:seeAlso !<http://www.infectiousdiseaseontology.org/Home.html>)
       (ontology-annotation !dc:creator "Lindsay Cowell")
       (ontology-annotation !dc:creator "Albert Goldfain")
       (ontology-annotation !dc:contributor "Barry Smith")
       (ontology-annotation !dc:contributor "Alan Ruttenberg")
       (ontology-annotation !<http://protege.stanford.edu/plugins/owl/protege#defaultLanguage> "en")
       (annotation-property !obo:IAO_0000115 (label "definition"))
       (annotation-property !obo:IAO_0000117 (label "definition editor"))
       (annotation-property !obo:IAO_0000119 (label "definition source"))
       (annotation-property !obo:IAO_0000116 (label "editor note"))
       (annotation-property !obo:IAO_0000118 (label "alternative term"))
       (annotation-property !obo:OBI_0000283 (label "imported from"))
       (annotation-property !<http://protege.stanford.edu/plugins/owl/protege#defaultLanguage>)
       (object-property !oborel:part_of)
       (ontology-annotation !dc:date (literal "2009-08-13" !xsd:date))
       (class !obo:IDO_9999999 (label "_defined class") :partial
	      (annotation !obo:IAO_0000116 "placeholder"))
       (loop for (term id) in *ido-defined-classes* collect
	    (class id (label (literal (format nil "_defined ~a" term) :|@en|))
		   (annotation !obo:IAO_0000116 "placeholder")
		   :partial
		   (or (gethash term *ido-term-to-uri*)
		       (second (assoc term *ido-external-terms* :test 'equalp)))
		   )
	    collect
	    (class id :partial !obo:IDO_9999999 )
	    )
       (class !<http://www.ifomis.org/bfo/1.1/snap#Capability> :partial !<http://www.ifomis.org/bfo/1.1/snap#RealizableEntity>)
       (class !<http://purl.obofoundry.org/obo/GO#GO_0005575> :partial !<http://www.ifomis.org/bfo/1.1/snap#MaterialEntity>
	      (label "cellular component"))
       (class !<http://purl.obofoundry.org/obo/GO#GO_0005615> :partial !<http://www.ifomis.org/bfo/1.1/snap#Site>
	      (label "extracellular space")
	      (restriction !oborel:part_of (some-values-from !obo:OBI_0100026)))
       (class !obo:OBI_0100026 :partial !snap:MaterialEntity
	      (label (literal "organism" :|@en|))
	      (annotation !<http://purl.obolibrary.org/obo/OBI_0000283> !<http://purl.obolibrary.org/obo/obi.owl>)
	      (annotation !obo:IAO_0000115 "An organism is material entity that is an individual living system, such as animal, plant, bacteria or virus, that is capable of replicating or reproducing, growth and maintenance in the right environment. An organism may be unicellular or made up, like humans, of many billions of cells divided into specialized tissues and organs."))
       (loop for entry in termdefs
	  for term = (second (assoc :term entry))
	  for uri = (gethash term *ido-term-to-uri*)
	  for definedclass? = (equal (second (assoc :sheet entry)) "Defined Classes")
	  for isa-uri =
	  (cond (definedclass?
		 (second (assoc (second (assoc :is_a entry)) *ido-defined-classes* :test 'equalp)))
		(t (or (gethash (second (assoc :is_a entry)) *ido-term-to-uri*)
		       (second (assoc (second (assoc :is_a entry))
				      *ido-external-terms* :test 'equalp)))))
	  for definition = (second (assoc :definition entry))
	  for is_a = (second (assoc :is_a entry))
	  for synonyms = (second (assoc :synonym entry))
	  when (and term (not (equal term "")))
	  append
	    (if  (and
		  (not definedclass?)
		  (not (and uri isa-uri)))
	       (progn (warn "Whoops - need uri (~a) and isa-url (~s) - ~a" uri isa-uri entry)
		      nil)
	       (list (apply 'class uri :partial isa-uri
			    (label (literal term :|@en|))
			    (annotation !obo:IAO_0000117 "Lindsay Cowell")
			    (annotation !obo:IAO_0000117 "Albert Goldfain")
			    (when definition (annotation !obo:IAO_0000115 (literal (safe-annotation-string definition 'definition) :|@en|)))
			    (loop for syn in synonyms collect  
				 (annotation !obo:IAO_0000118 (literal syn :|@en|)))))
	       )))
;    (princ (abstract-syntax ido))
    (write-rdfxml ido)))

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

(defun historical-ido-terms ()
  (let ((kb (load-kb-jena "ido:ido-core;historical;IDO-1-3-oboconv.owl")))
    (let ((*current-labels* (rdfs-labels kb)))
      (mapcar (lambda(e)
		(list (#"replaceAll" (#"replaceAll" (uri-full (first e)) ".*#" "") "_" ":")
p		      (second e)))
	      (loop for class in
		   (remove-if-not (lambda(e) (search "IDO#" (uri-full e)))
				  (descendants !owl:Thing kb))
		   collect (list class (gethash class *current-labels*)))))))

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
					;"active immunization against smallpox" 
     ("process" !span:Process)
     ("disease course(OGMS:0000063)" !obo:OGMS_0000063)
     ("material entity that is alive" !obo:OBI_0100026) ; organism
;     "organism that has the capability to bear the host role" 
     ("material entity" !<http://www.ifomis.org/bfo/1.1/snap#MaterialEntity>)
     ("organism"   !obo:OBI_0100026)
;     "organism that is the bearer of the host of infectious parasite role" 
;     "organism that is the bearer of the host role" 
;     "organism that is the bearer of the host of infectious agent role" 
;     "organism with an immune system"
;     "organism that has the pathogenic disposition" 
;     "organism that has the infectious disposition" 
     ("quality"  !<http://www.ifomis.org/bfo/1.1/snap#Quality>)
     ("disposition"  !<http://www.ifomis.org/bfo/1.1/snap#Disposition>)
     ("occurrent"  !<http://www.ifomis.org/bfo/1.1/span#Occurrent>)
     ("object"  !<http://www.ifomis.org/bfo/1.1/snap#Object>)
     ("disease" !obo:OGMS_0000063)
     ("disorder" !obo:OGMS_0000045)
     ("capability" !<http://www.ifomis.org/bfo/1.1/snap#Capability>)

     ("quality?" !<http://www.ifomis.org/bfo/1.1/snap#Quality>)
     ("site?" !<http://www.ifomis.org/bfo/1.1/snap#Site>)
     ("site" !<http://www.ifomis.org/bfo/1.1/snap#Site>)
     ("colonized quality?" !obo:IDO_0000459)
     ("infected quality?" !obo:IDO_0000460)
     ("role" !<http://www.ifomis.org/bfo/1.1/snap#Role>)
     ("cellular component" !<http://purl.obofoundry.org/obo/GO#GO_0005575>)
     ("extracellular space" !<http://purl.obofoundry.org/obo/GO#GO_0005615>)
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
    ("immunity to infectious organism" !obo:IDO_9099990))))