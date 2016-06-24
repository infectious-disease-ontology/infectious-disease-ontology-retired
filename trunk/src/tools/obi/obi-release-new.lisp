;; trim the subclass hierarchy by creating a new ontology with only
;; the asserted and inferred subclass relations, classifying it, and
;; then using the pellet parent function to get the most direct
;; superclass of each class.

(defun necessary-subclass-assertions (kb)
  (let ((table (make-hash-table :test 'equalp)))
    (with-ontology subclass-only ()
	((loop for (s o) in
	      (sparql '(:select (?s ?o)()
			(?s !rdfs:subClassOf ?o)
			(:filter (and (not (isblank ?s)) (not (isblank ?o)))))
		      :kb kb :use-reasoner :none)
	    append (class s :partial o)))
      (#"classify" (kb-kb subclass-only))
      (loop for class in (descendants !owl:Thing subclass-only)
	   do
	   (loop for parent in (parents class subclass-only)
		do (setf (gethash (list (uri-full class) (uri-full parent)) table) t)))

      table)))

(defun ontology-names (kb)
  (mapcar 'uri-full
	  (sparql '(:select (?s)()
	    (?s !rdf:type !owl:Ontology))
	  :kb kb :use-reasoner :none :flatten t)))

(defun create-bioportal-obi (dest obi &optional last)
  (create-combined-obi dest obi :include-all-imports last))

;; creating a file for release
;; Two passes:
;; First combine everything into one file, so that the subclasses can be extracted and a clean hierarchy created
;; Second use that and do other cleanup

(defun create-combined-obi (dest obi type &key
			    (version :release) ;; merges in everything but the ones we externally import. Alternative: :include-all-imports t
			    pretty ;; what the label for the doap:release should be
			    last ;; not for user use. Controls first or second pass.
			    (drop-curator-note t) ;; whether to drop curator notes
			    (copy-preflabel t) ;; whether to copy rdfs:labelt to !'editor preferred label'@obi
			    (remove-obsolete-classes t) ;; whether to remove obsolete classes
			    (use-iao-main nil) ;; whether to use iao-main.owl versus iao.owl
			    (use-iao-version nil)) ;; supply YYYY-MM-DD if you want to import a specific version of IAO
  (let* ((kb (load-kb-jena obi))
	 (imported (remove "http://" (imported kb) :test 'search))
	 (out-model (create-empty-obi-model))
	 (curation-note-uri "http://purl.obolibrary.org/obo/IAO_0000232")
	 (necessary-subclass-assertions (and last (necessary-subclass-assertions (load-kb-jena last))))
	 (imported-ontology-names (remove "http://purl.obolibrary.org/obo/obi.owl" (ontology-names kb) :test 'equal)))
    (if last
	(format t "~%Second pass~%")
	(format t "~%First pass~%"))
    (loop for file in (if (or (not last) (eq type :include-all-imports))
			  imported
			  (remove-if-not (lambda(e) (search "/src/ontology/branches/" e)) imported))
       for in-model = (#"createDefaultModel" 'modelfactory)
       with dont = (list*
		    (mapcar 'uri-full (sparql `(:select (?term) ()
						(:union
						 ((?term !rdfs:label ?label)
							 (:filter (regex (str ?label) "^_.*")))
						 ,@(and remove-obsolete-classes
						       (list '((?term !rdfs:subClassOf !<http://www.geneontology.org/formats/oboInOwl#ObsoleteClass>))))
						 ,@(and remove-obsolete-classes
						       (list '((?term !rdfs:subPropertyOf !<http://www.geneontology.org/formats/oboInOwl#ObsoleteProperty>))))))
					      :kb kb :use-reasoner :none :flatten t)))
       do
       (format t "reading ~a~%" file)
       (#"read" in-model
		(new 'bufferedinputstream
		     (#"getInputStream" (#"openConnection" (new 'java.net.url file))))
		"http://purl.obolibrary.org/obo/obi.owl")
       (loop with iterator = (#"listStatements" in-model)
	  while (#"hasNext" iterator)
	  for statement = (#"next" iterator)
	  for subject = (#"getSubject" statement)
	  for object = (#"getObject" statement)
	  for predicate = (#"toString" (#"getURI" (#"getPredicate" statement)))

	  ;; remove some set of triples
	  unless (or

		  ;; remove imports. We will add them back. The only isDefinedBy we will have will be from doap.
		  (member predicate '("http://www.w3.org/2002/07/owl#imports" "http://www.w3.org/2000/01/rdf-schema#isDefinedBy")
			  :test 'equal)

		  ;; remove curator notes
		  (and drop-curator-note (equal predicate curation-note-uri))

		  ;; remove obsolete classes and classes that start with "_". (aka "dont")
		  ;; remove any annotations on imported/included ontologies.
		  (and (not (#"isAnon" subject) )
		       (or (member (#"toString" (#"getURI" subject)) dont :test 'equal)
			   (member (#"toString" (#"getURI" subject)) imported-ontology-names :test 'equal)))

		  ;; remove any ontology "headers". There will be a single one added - that for OBI.
		  (and (equal predicate "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
		       (equal (#"toString" (#"getURI" object)) "http://www.w3.org/2002/07/owl#Ontology"))

		  ;; if we are mirroring rdfs:label then get rid of old editor preferred label annotations
		  (and copy-preflabel (equal predicate "http://purl.obolibrary.org/obo/IAO_0000111"))

		  ;; on the second pass, only include subclass relations that are needed - cleans up the display
		  ;; of hierarchy for tools that don't otherwise do so

		  (and last
		       (equal predicate "http://www.w3.org/2000/01/rdf-schema#subClassOf")
		       (not (#"isAnon" subject))
		       (not (#"isAnon" object))
		       (not (gethash (list (#"toString" (#"getURI" subject)) (#"toString" (#"getURI" object)))
				     necessary-subclass-assertions)))
		  ;; remove any references to obsolete classes and classes that start with "_". (aka "dont")
		  (and (#"isResource" object)
		       (not (#"isAnon" object))
		       (member (#"toString" (#"getURI" object)) dont :test 'equal)))
	  ;; if we pass the gauntlet, add to the new model
	  do (#"add" out-model statement)

	  ;; if we are mirroring rdfs:label then add editor preferred label annotations
	    (when (and last copy-preflabel
		       (equal predicate "http://www.w3.org/2000/01/rdf-schema#label"))
	      (add-jena-triple out-model subject "http://purl.obolibrary.org/obo/IAO_0000111" object))
	    ))
	 
    ;; now add back in some triples.

    ;; Ontology header
    (add-jena-triple out-model "http://purl.obolibrary.org/obo/obi.owl" !rdf:type !owl:Ontology)

    ;; Explicit imports
    (when (and last (eq type :release))
      (loop for source in (list (if use-iao-main
				    (if use-iao-version
					(make-uri (format nil "http://purl.obolibrary.org/obo/iao/~a/iao-main.owl" use-iao-version))
					!<http://purl.obolibrary.org/obo/iao/iao-main.owl>)
				    (if use-iao-version
					(make-uri (format nil "http://purl.obolibrary.org/obo/iao/~a/iao.owl" use-iao-version))
					!<http://purl.obolibrary.org/obo/iao.owl>))
				!<http://www.obofoundry.org/ro/ro.owl>
				!<http://www.ifomis.org/bfo/1.1>
				!<http://protege.stanford.edu/plugins/owl/dc/protege-dc.owl>
				!<http://purl.org/obo/owl/ro_bfo_bridge/1.1>)
	   do (add-jena-triple out-model "http://purl.obolibrary.org/obo/obi.owl" !owl:imports source)))

    ;; See http://see obi-ontology.org/page/Releases/How_OBI_is_versioned
    (when last
      ;; DOAP - description of a project
      (add-doap out-model version pretty)
      ;; The versionInfo is date, to granularity of day
      (add-jena-triple out-model "http://purl.obolibrary.org/obo/obi.owl" !owl:versionInfo version)
      ;; the versionIRI is constructed out of the date.
      (add-jena-triple out-model "http://purl.obolibrary.org/obo/obi.owl" !owl:versionIRI
		       (format nil "http://purl.obolibrary.org/obo/obi/~a/obi.owl" version)))

    (if last
	;; second pass. we're done
	(write-jena-model out-model (namestring (translate-logical-pathname dest)))
	;; first pass - write it out, and rerun with :last dest, so that necessary subclasses can be computed.
	(progn
	  (write-jena-model out-model (namestring (translate-logical-pathname dest)))
	  (create-combined-obi dest obi type :last dest :pretty pretty :version version
			       :use-iao-main use-iao-main :use-iao-version use-iao-version
			       :copy-preflabel copy-preflabel :remove-obsolete-classes remove-obsolete-classes
			       )
	  ))))

;; See http://see obi-ontology.org/page/Releases/How_OBI_is_versioned
(defun add-doap (model version pretty-name &optional (doap "obi:branches;doap.template"))
  (with-open-file (newdoap "/tmp/doap.owl" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (with-open-file (f doap)
      (loop for line = (read-line f nil :eof)
	 until (eq line :eof)
	 do (write-line (#"replaceFirst" (#"replaceFirst"  line "__VERSION_GOES_HERE__" version) "__PRETTY_NAME_GOES_HERE__" pretty-name)
			newdoap))))
  (let ((in-model (create-empty-obi-model)))
    (#"read" in-model
	     (new 'bufferedinputstream
		  (#"getInputStream" (#"openConnection" (new 'java.net.url "file:///tmp/doap.owl" ))))
	     "http://purl.obolibrary.org/obo/obi.owl")
    (loop with iterator = (#"listStatements" in-model)
       while (#"hasNext" iterator)
       for statement = (#"next" iterator)
       do (#"add" model statement))))

(defun dump-strings (ontology file
		     &optional
		     (props (list (list !rdfs:label "label")
				  (list !'definition'@obi "definition")
				  (list !'editor note'@obi "note")))
		     )
  (with-open-file (f file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((in-model (create-empty-obi-model)))
      (#"read" in-model
	       (new 'bufferedinputstream
		    (#"getInputStream" (#"openConnection"
					(new 'java.net.url
					     (format nil "file://~a" (namestring (translate-logical-pathname ontology)))))))
	       "http://purl.obolibrary.org/obo/obi.owl")
      (let ((dump-props
	     (mapcar (lambda(e) (uri-full (car e))) props)))
	(loop with iterator = (#"listStatements" in-model)
	   while (#"hasNext" iterator)
	   for statement = (#"next" iterator)
	   for subject = (#"getSubject" statement)
	   for object = (#"getObject" statement)
	   for predicate = (#"toString" (#"getURI" (#"getPredicate" statement)))
	   do
	   (when (and (member predicate dump-props :test 'equal)
		      (search "OBI_" (#"toString" (#"getURI" subject))))
	     (format f "~a,~a,~s~%"
		     (#"replaceFirst" (#"toString" (#"getURI" subject)) ".*[#/]" "")
		     (second (assoc (make-uri predicate) props))
		     (#"replaceAll"  (#"toString" (#"getString" object)) "\"" "\"\""))))))))

(defun spell-fix-file (in out)
  (let ((foundpattern
	 (#"compile" 'java.util.regex.Pattern
		     (with-output-to-string (s)
		       (format s "(.*?)\\b(")
		       (loop for (bad) in *obi-spelling-fixme*
			  do (format s "(~a)|" bad))
		       (format s "(bhufua))\\b(.*)") ; doesn't occur
		       ))))
    (with-open-file (fin in)
      (with-open-file (fout out :direction :output :if-exists :supersede :if-does-not-exist :create)
	(loop for line = (read-line fin nil :eof)
	   until (eq line :eof)
	   for new = nil
	   do
	   (if (#"matches"  (#"matcher" foundpattern line))
	       (progn
		 (loop for (bad good) in *obi-spelling-fixme*
		    do (setq new (#"replaceFirst" (or new line) (format nil "(\\b~a\\b)" bad) good)))
		 (setq new (#"replaceAll" new "  " " "))
		 (write-line new fout))
	       (write-line line fout))
	   )))))
      
(defun entity-report (kb &optional (reasoner :none))
  (let ((class-prefixes 
	 (set-difference
	  (remove-duplicates 
	   (mapcar (lambda(e) (#"replaceFirst" (uri-full e) "(.*)[#/_].*" "$1"))
		   (sparql '(:select (?class) () 
			     (?class !rdf:type !owl:Class)(:filter (not (isblank ?class))))
			   :kb kb
			   :use-reasoner reasoner :flatten t))
	   :test #'equal)
	  '("http://www.ifomis.org/bfo/1.1/snap" "http://www.ifomis.org/bfo/1.1/span")
	  :test 'equal)
	  )
	(property-prefixes 
	 (list*
	  "http://purl.obolibrary.org/obo/OBI"
	  "http://purl.obolibrary.org/obo/IAO"
	  (remove "http://purl.obolibrary.org/obo"
		  (remove-duplicates 
		   (mapcar (lambda(e) (#"replaceFirst" (uri-full e) "(.*)?[#/].*" "$1"))
			   (sparql '(:select (?prop) () 
				     (:union
				      ((?prop !rdf:type !owl:DatatypeProperty))
				      ((?prop !rdf:type !owl:AnnotationProperty))
				      ((?prop !rdf:type !owl:ObjectProperty)))
				     )
				   :kb kb
				   :use-reasoner reasoner :flatten t))
		   :test #'equal) :test 'equal)))
	)
    (loop for prefix in class-prefixes
       for count = 
       (sparql `(:select (?class) (:count t :distinct t) 
			 (?class !rdf:type !owl:Class)
			 (:filter (regex (str ?class) ,prefix)))
	       :kb kb
	       :use-reasoner reasoner)
       do (format t "Class	~a ~a~%" (#"replaceFirst"  (#"replaceFirst" prefix ".*[#/_]" "") "1.1" "BFO") count)
       sum count)
    (loop for prefix in property-prefixes
       for count = 
       (sparql `(:select (?prop) (:count t :distinct t) 
			 (:union
			  ((?prop !rdf:type !owl:DatatypeProperty))
			  ((?prop !rdf:type !owl:AnnotationProperty))
			  ((?prop !rdf:type !owl:ObjectProperty)))
			 (:filter (regex (str ?prop) ,prefix)))
	       :kb kb
	       :use-reasoner reasoner)
       do (format t "Property	~a ~a~%" (#"replaceFirst"  (#"replaceFirst" prefix ".*[#/](.*?)(\\..*){0,1}$" "$1") "1" "dc") count)
       sum count)
    ))

(defparameter *obi-spelling-fixme*
  '(("ploymerization" "polymerization")
    ("decompostion" "decomposition ")
    ("extration" "extraction")
    ("outputing" "outputting")
    ("combininig" "combining ")
    ("charactreristic" "characteristic")
    ("flourescent" "fluorescent")
    ("inputing" "inputting")
    ("emmitted" "emitted")
    ("subsytem" "subsystem")
    ("choise" "choice")
    ("matterial" "material")
    ("may may" "map ")
    ("iin" "in")
    ("microtitre" "microtiter")
    ("partical" "particle")
    ("functionto" "function to")
    ("dectors" "detectors")
    ("entitiy" "entity")
    ("disolved" "dissolved ")
    ("emmission" "emission")
    ("process,the" "process, the ")
    ("transformationi" "transformation")
    ("passiv," "passive,")
    ("powerfull" "powerful")
    ("adminstration" "administration")
    ("viceversa" "vice versa")
    ("speciment" "speciment ")
    ("subjeceted" "subjected ")
    ("ie" "i.e.")
    ("occurence" "occurrence")
    ("portionn" "portion")
    ("portio" "portion")
    ("enabeling" "enabling ")
    ("indegree" "in-degree")
    ("outdegree" "out-degree")
    ("aquire" "acquire ")
    ("aquisition" "acquisition ")
    ("extraneuous" "extraneous")
    ("differrent" "different ")
    ("quantom" "quantum ")
    ("wih" "with ")
    ("partioning" "partitioning")
    ("carrie" "carry ")
    ("chromaptgraphy" "chromatography")
    ("dicusssion" "discussion")
    ("multicelluar" "multicellular")
    ("arive" "arrive ")
    ("volatalization" "volatilization")
    ("calulcation" "calculation")
    ("introcue" "introduce ")
    ("GenePatter" "GenePattern ")
    ("nmr" "NMR")
    ("environmentl" "environmental")
    ("binarydigital" "binary digital")
    ("biais" "bias ")
    ("datapoints" "data points ")
    ("exmination" "examination ")
    ("repliccate" "replicate ")
    ("crystaline" "crystalline ")
    ("flashlamp" "flash lamp ")
    ("assymetric" "asymmetric ")
    ("consituant" "constituent ")
    ("detoxification,hemotapoeietic" "detoxification, hematopoietic ")
    ("proceses" "processes ")
    ("widebore" "wide bore")
    ("ocurring" "occurring")
    ("possiby" "possibly")
    ("the the" "the ")
    ("enitity" "entity")
    ("assymmetric" "asymmetric")
    ("modelling" "modeling")
    ("froma" "from a")
    ("polynommial" "polynomial")
    ("continuos" "continuous")
    ("colum" "column ")
    ("metalic" "metallic ")
    ("An cation" "A cation")
    ("cristal" "crystal ")
    ("irratidates" "irradiates ")
    ("cyttometer" "cytometer ")
    ("relaized" "realized ")
    ("nuceic" "nucleic")
    ("digitiesed" "digitized ")
    ("consumableis" "consumable is")
    ("bearning" "bearing ")
    ("recominant" "recombinant ")
    ("moleculeany" "molecule any")
    ("detecor" "detector ")
    ("alalog" "analog ")
    ("diffusiton" "diffusion ")
    ("inpact" "impact ")
    ("lourescent" "fluorescent")
    ("emmited" "emitted ")
    ("focussing" "focusing ")
    ("separateion" "separation ")
    ("freqeuntly" "frequently ")
    ("nitorgen" "nitrogen")
    ("anaylsis" "analysis")
    ("confered" "conferred ")
    ("vetcor" "vector")
    ("odf" "of ")
    ("cromatography" "chromatography")
    ("independant" "independent")
    ("diagnsis" "diagnosis")
    ("oestrogen" "estrogen")
    ("investivation" "investigation")
    ("organislm" "organism ")
    ("synthezised" "synthesized")
    ("mixttures" "mixtures")
    ("indefinately" "indefinitely")
    ("hiearchical" "hierarchical")
    ("clustring" "clustering ")
    ("trancsripts" "transcripts")
    ("calulcation" "calculation")
    ("fluorescnce" "fluorescence")
    ("pulsewidths" "pulse widths ")
    ("synringe" "syringe ")
    ("connected,attached" "connection, attached")
    ("ouput" "output ")
    ("conmtains" "contains ")
    ("files.They" "files. They ")
    ("hierarchicaly" "hierarchically ")
    ("inividal" "individual")
    ("artifical" "artificial ")
    ("materila" "material ")
    ("outpu" "output ")
    ("flouresce" "fluoresce")
    ("availalbe" "available")
    ("tthat" "that ")
    ("ultaimtely" "ultimately ")
    ("gaz" "gas")
    ("FIlters" "Filters")
    ("acidsl" "acids")
    ("bandpass" "band-pass")
    ("insturment" "instrument")
    ("proprty" "property")
    ("wellplate" "well plate")
    ("hastaloy" "hastelloy")
    ("photodector" "photodetector")
    ("cells.This" "cells. This ")
    ("interogation" "interrogation")
    ("occurent" "occurrent")
    ("parametersand" "parameters and")
    ("nonrepeatable" "non-repeatable ")
    ("artifically" "artificially")
    ("specifc" "specific")
    ("Differrences" "Differences")
    ("affinnity" "affinity")
    ("seive" "sieve")
    ("proprotion" "proportion")
    ("conainted" "contained")
    ("celll" "cell")
    ("in heres" "inheres")
    ("artefact" "artifact ")
    ("constituants" "constituents ")
    ("tissues.Values" "tissues. Values")
    ("occuring" "occurring ")
    ("concretisation" "concretization")
    ("measuremeant" "measurement")
    ("electropheris" "electrophoresis")
    ("fiberoptics" "fiber optics")
    ("approximateed" "approximated")
    ("comparewd" "compared")
    ("logicle" "logical ")
    ("aggegration" "aggregation ")
    ("differntial" "differential")
    ("collector.Guava" "collector. Guava ")
    ("acomplish" "accomplish")
    ("techique" "technique")
    ("dischargeing" "discharging ")
    ("amplication" "amplification ")
    ("assayis" "assay is ")
    ("distriubition" "distribution ")
    ("colelction" "collection ")
    ("aplications" "applications")
    ("oragnism" "organism")
    ("reproter" "reporter")
    ("strucure" "structure")
    ("techically" "technically")
    ("mutation,deletion" "mutation, deletion")
    ("proportioal o" "proportional to")
    ("intermitent" "intermittent")
    ("realtime" "real time")
    ("hightest" "highest")
    ("recieves" "receives ")
    ("implict" "implicit")
    ("anlysis" "analysis")
    ))

