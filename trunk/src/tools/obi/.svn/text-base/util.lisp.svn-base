(defvar *obi-uri-pattern* (#"compile" 'java.util.regex.pattern "^http://purl.(obolibrary|obofoundry).org/obo/OBI_\\d+$"))

(defun is-obi-uri (uri)
  (#"matches" (#"matcher" *obi-uri-pattern* (uri-full uri))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-uri-alias "material-entity" !snap:MaterialEntity)
  (def-uri-alias "ready-for-release" !<http://purl.obolibrary.org/obo/IAO_0000122>)
  (def-uri-alias "metadata-complete" !<http://purl.obolibrary.org/obo/IAO_0000120>)
  (def-uri-alias "metadata-incomplete" !<http://purl.obolibrary.org/obo/IAO_0000123>)
  (def-uri-alias "pending-final-vetting" !<http://purl.obolibrary.org/obo/IAO_0000125>)
  (def-uri-alias "uncurated" !<http://purl.obolibrary.org/obo/IAO_0000124>)
  (def-uri-alias "placeholder" !<http://purl.obolibrary.org/obo/IAO_0000121>)
  (def-uri-alias "obsolete-class" !<http://www.geneontology.org/formats/oboInOwl#ObsoleteClass>)
  (def-uri-alias "definition" !obi:IAO_0000115)
  (def-uri-alias "definition-source" !obi:IAO_0000119)
  (def-uri-alias "definition-editor" !obi:IAO_0000117)
  (def-uri-alias "preferred-term" !obi:IAO_0000111)
  (def-uri-alias "alternative-term" !obi:IAO_0000118)
  (def-uri-alias "example-of-usage" !obi:IAO_0000112)
  (def-uri-alias "curation-status" !obi:IAO_0000078)
  (def-uri-alias "editor-note" !obo:IAO_0000116)
  (def-uri-alias "curator-note" !obo:IAO_0000232)
  )


(defmacro with-obo-metadata-uris (&body body)
  `(let-uri (("ready-for-release" !obo:IAO_0000122)
	     ("metadata-complete" !obo:IAO_0000120)
	     ("metadata-incomplete" !obo:IAO_0000123)
	     ("pending-final-vetting" !obo:IAO_0000125)
	     ("uncurated" !obo:IAO_0000124)
	     ("placeholder" !obo:IAO_0000121)
	     ("obsolete-class" !<http://www.geneontology.org/formats/oboInOwl#ObsoleteClass>)
	     ("definition" !obo:IAO_0000115)
	     ("definition-source" !obo:IAO_0000119)
	     ("definition-editor" !obo:IAO_0000117)
	     ("preferred-term" !obo:IAO_0000111)
	     ("alternative-term" !obo:IAO_0000118)
	     ("example-of-usage" !obo:IAO_0000112)
	     ("has-curation-status" !obo:IAO_0000114)
	     ("editor-note" !obo:IAO_0000116)
	     ("curator-note" !obo:IAO_0000232)
	     ("curation-status" !obo:IAO_0000078)
	     ("imported-from" !obo:IAO_0000412)
	     )
	    ,@(loop for p in (list !obo:IAO_0000112 !obo:IAO_0000118 !obo:IAO_0000111 !obo:IAO_0000117 !obo:IAO_0000232
				   !obo:IAO_0000119 !obo:IAO_0000115 !obo:IAO_0000412 !obo:IAO_0000114 !obo:IAO_0000116)
		 collect `(annotation-property ,p))
	    ,@(loop for i in (list !obo:IAO_0000122 !obo:IAO_0000120 !obo:IAO_0000123 !obo:IAO_0000125 !obo:IAO_0000124 !obo:IAO_0000121) 
		 collect `(individual ,i (type !curation-status)))
	    ,@body
	    ))

(defvar *obi-label-source* nil)

(defmethod make-uri-from-label-source ((source (eql :obi)) name actual)
  "URI label source for OBI. All labels in OBI or imports are available. Case insensitive matching. Complains if an ambiguous label is used, or if a label is missing, or if actual is supplied and doesn't match the looked up term"
  (unless *obi-label-source*
    (let ((table (make-hash-table :test 'equalp)))
      (let ((kb (load-kb-jena :obim)))
      (let ((labels (rdfs-labels kb)))
	(maphash (lambda(uri label) 
		   (if (gethash label table) 
		       (unless (eq (gethash label table)  uri)
			 (print-db (gethash label table) uri label)
			 (setf (gethash label table) :ambiguous))
		       (setf (gethash label table) uri)))
		 labels))
      (setq *obi-label-source* table))))
  (let ((found (gethash name *obi-label-source*)))
    (when (eq found :ambiguous)
      (if (equalp name "homo sapiens") ; ugly special case until it's fixed.
	  (return-from make-uri-from-label-source !taxon:9606)
	  (progn
	    (warn "Uri label ~a in ~a is ambiguous" name source)
	    (setq found nil))))
    (if found
	(progn
	  (when actual
	    (assert (eq found (make-uri nil actual)) (found actual)
		    "Uri label lookup for '~a' - ~a doesn't match specified actual '~a'"
		    name found actual))
	  found)
	(if actual
	    (progn
	      (warn "Uri label lookup for '~a' failed - using provided actual: '~a'" name actual)
	      (make-uri nil actual))
	    (error "Couldn't determine which URI was meant by '~a' in ~a" name source)))))
  
  