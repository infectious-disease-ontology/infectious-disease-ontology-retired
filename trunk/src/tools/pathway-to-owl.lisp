(defvar *ido-term-to-uri* (make-hash-table :test 'equalp))

(defmethod handle-uri :around ((o parsed-handle))
  (or (call-next-method) (setf (handle-uri o) (compute-handle-uri o))))

(defun legacy-uri (prefix id handle)
  (cond ((#"matches" id "^\\d+$")
         (make-uri (format nil "http://purl.org/obo/owl/~a#~a_~a" prefix prefix id)))
	((equal id "submitted")
	 (make-uri (make-uri (format nil "http://purl.org/obo/owl/~a#submitted_~a" prefix (string-downcase handle)))))
	(t (error "Don't know how to make uri for ~a:~a (~a)" prefix id handle))))

(defun obolibrary-uri (prefix id handle)
  (cond ((#"matches" id "^\\d+$")
         (make-uri (format nil "http://purl.obolibrary.org/obo/~a_~a" prefix id)))
	((equal id "submitted")
	 (make-uri (make-uri (format nil "http://purl.obolibrary.org/obo/~a_submitted_~a" prefix (string-downcase handle)))))
	(t (error "Don't know how to make uri for ~a:~a (~a)" prefix id handle))))

(defun pfam-defined-protein-part-uri (id)
  (make-uri (format nil "http://purl.obolibrary.org/obo/PF_~a"  id)))

(defmethod compute-handle-uri ((o parsed-handle))
  (let ((handle (handle o))
	(desc (handle-description o))
	(type (handle-kind o))
	(id (handle-class o))
	(super (handle-super o)))
    (flet ((compute-uri (base prefix id handle)
	     (cond ((#"matches" id "^\\d+$")
		    (make-uri (format nil "http://purl.obolibrary.org/obo/~a_~a" prefix id)))
		   ((equal id "submitted")
		    (make-uri (format nil "http://purl.obolibrary.org/obo/~a_submitted_~a" prefix (string-downcase handle))))
		   (t (error "")))))
      (unless (null id)
	(destructuring-bind (prefix id) (or (car (all-matches id "(.*):(.*)" 1 2))
					    (car (all-matches id "(PF)(.*)" 1 2))) ; handle PF without "PFAM" prefix.
	  (cond ((equal prefix "GO")
		 (assert (or (#"matches" id "^\\d+") (equal id "submitted")) (id) "GO id GO:~a malformed" id)
		 (legacy-uri "GO" id handle)) 
		((equal prefix "PRO")
		 (legacy-uri "PRO" id handle))
		((equal prefix "MI")
					;("compr" "competitor" "role" "MI:0941" NIL) 
					;("compb" "competition binding" "process" "MI:0405" NIL) 
		 ;; not used
		 (warn "Not translating MI term MI:~a" id)
		 )
		((equal prefix "PMID") (warn "~a ~a" handle id))
		((equal prefix "SO")
		 (legacy-uri "SO" id handle)
		 ;; http://www.cbrc.jp/htbin/bget_tfmatrix?M00258 - ISRE. What gene is ISRE upstream of?
					;("ISREdnas" "double strand dna sequence transcript_bound_by_protein" "double strand dna sequence   bound by protein" "SO:0000279" NIL)
					; fixme
		 )
		((or (equal prefix "PF") (equal prefix "PFAM"))
		 (pfam-defined-protein-part-uri (#"replaceFirst" id "PF" "")))
		((equal prefix "PATO")
		 (legacy-uri "PATO" id handle))
		((equal prefix "MOD"))
					; ("Thrp" "O-phospho L-threonin" "modified aminoacid" "MOD:00047" NIL)  -> CHEBI:37525
		((equal prefix "CHEBI")
		 (legacy-uri "CHEBI" id handle)
		 )
		(t (error ""))))))))

(defmethod term-ontology ((o parsed-handle))
  (let ((name (caar (all-matches (uri-full (handle-uri o)) "(GO|SO|PATO|CHEBI|PRO)(_|#)" 1 2))))
    (and name
	 (return-from term-ontology (values (make-uri (format nil "http://purl.org/obo/owl/~a" name)) name))))
  (let ((name (caar (all-matches (uri-full (handle-uri o)) "(IDO|PFAM|OGMS)_" 1))))
    (and name
	 (values (make-uri (format nil "http://purl.obolibrary.org/obo/~a" name)) name))))

(defparameter *mireot-parent-terms*
  '((:protein "PRO" !oboont:PRO#PRO_000000001 !snap:MaterialEntity)
    (:molecular-function "GO" !oboont:GO#GO_0003674 !snap:Function)
    (:cellular-component "GO" !oboont:GO#GO_0005575 !snap:MaterialEntity)
    (:biological-process "GO" !oboont:GO#GO_0008150 !span:ProcessualEntity)
    (:molecular-entity "CHEBI" !oboont:CHEBI#CHEBI_23367 !snap:MaterialEntity)
    (:domain "PFAM" !oboont:PRO#PRO_000018263 !snap:MaterialEntity)  ; amino acid chain. Should there be a term "domain"? SO:0000417 is POLYPEPTIDE_DOMAIN
    (:quality "PATO" !snap:Quality)
    (:protein-complex "GO" !oboont:GO#GO_0043234 !snap:MaterialEntity)
    (:protein-complex "SO" !oboont:GO#GO_0032991 !snap:MaterialEntity) ; for now only one
    ))



(defmethod mireot-parent-term ((o parsed-handle))
  (multiple-value-bind (ont-uri ont-name) (term-ontology o)
    (cond ((member ont-name '("PRO" "CHEBI" "PFAM" "PATO" "SO") :test 'equal)
	   (third (find ont-name *mireot-parent-terms* :test 'equal :key 'second)))
	  ((equal ont-name "GO")
	   (let ((type (handle-kind o)))
	     (cond ((member type '("function" "molecular function") :test 'equalp)
		    (third (assoc :molecular-function *mireot-parent-terms*)))
		   ((member type '("process" "biological process") :test 'equalp)
		    (third (assoc :biological-process *mireot-parent-terms*)))
		   ((member type '("CC" "GO CC" "cellular component") :test 'equalp)
		    (third (assoc :cellular-component *mireot-parent-terms*)))
		   ((equalp type "complex")
		    (third (assoc :protein-complex *mireot-parent-terms*)))
		   )))
	  (t (error "Don't know parent for ~a" o)))))

(defmethod write-external.owl ((book ido-pathway-book))
  (let ((axioms 
	 (loop for (kind where term parent) in *mireot-parent-terms*
	    unless (or (member where '("PFAM") :test 'equal)
		       (null parent))
	    append
	    `((declaration (class ,term))
	      (subclassof ,term ,parent)
	      (annotationassertion !obo:IAO_0000412 ,term ,(make-uri (format nil "http://purl.org/obo/owl/~a" where)))))))
    (foreach-row-in-block-type 
     book 'parsed-handle-block 
     (lambda(e)
       (unless (or (not (uri-p (handle-uri e)))
		   ;(equal (handle-class e) "submitted")
		   )
	 (multiple-value-bind (ont-uri ont-name) (term-ontology e)
	   (when (member ont-name '("CHEBI" "GO" "SO" "PATO" "PRO") :test 'equal)
	     (setq axioms
		   (append 
		    `((declaration (class ,(handle-uri e)))
		      (sub-class-of ,(handle-uri e) ,(mireot-parent-term e))
		      (annotation-assertion !obo:IAO_0000412 ,(handle-uri e) ,ont-uri)
		      (annotation-assertion !obo:IAO_0000116 ,(handle-uri e)
					    ,(format nil "from row ~a of ~a in sheet ~a from workbook ~a"
						     (in-row e)
						     (string-downcase (string (type-of (in-block e))))
						     (sheet-name (in-sheet (in-block e)))
						     (pathname-name (book-path (sheet-book (in-sheet (in-block e))))))))
		    (if (#"matches" (handle-class e) ".*submitted.*")
			`((annotation-assertion !rdfs:label ,(handle-uri e) ,(handle-description e))))
		    axioms)
		   ))))))
    (with-ontology external (:about !obo:ido/dev/pathway-external.owl :base !obo: :eval t)
	`((imports !obo:iao/ontology-metadata.owl) ,@axioms)
      (write-rdfxml external "ido:immunology;proto;pathway-external.owl"))))

;; doesn't yet run in owlapi3 - run in older

(defmethod create-externalDerived.owl ((book ido-pathway-book))
  (create-external-derived :kb (load-kb-jena "ido:immunology;proto;pathway-external.owl")
			   :output-path "ido:immunology;proto;pathway-external-derived.owl"
			   :templates-path "ido:tools;immunology-external-templates.txt"))

'(create-external-derived :kb (load-kb-jena "/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/immunology/proto/pathway-external.owl") :output-path "/Users/alanr/repos/infectious-disease-ontology/trunk/src/ontology/immunology/proto/pathway-external-derived.owl" :templates-path "~/repos/infectious-disease-ontology/trunk/src/tools/immunology-external-templates.txt" :ontology-uri (uri-full !obo:ido/dev/pathway-external-derived.owl))

(defmethod owl-axioms-for-processes ((book ido-pathway-book))
  (loop for bl in (blocks-of-type book 'parsed-process-block)
       append
       (loop for p in (parsed-rows bl)
	    append
	    (owl-axioms p))))

(def-uri-alias "realizes" !obi:OBI_0000308)
(def-uri-alias "substrate-disposition" !obi:IDO_0009001)
(def-uri-alias "product-disposition" !obi:IDO_0009002)
(def-uri-alias "inheres-in" !<http://purl.org/obo/owl/OBO_REL#inheres_in>)

(defmethod write-pathway.owl ((book ido-pathway-book))
  (with-ontology spreadsheet (:about !obo:ido/dev/pathway.owl :eval t)
      `((imports !obo:ido/dev/pathway-external.owl)
	(imports !obo:ido/dev/pathway-defs.owl)
	(imports !obo:ido/dev/pathway-external-derived.owl)
	(imports !bfo:)
	(imports !obo:iao/ontology-metadata.owl)
	(imports !oborel:)
	(declaration (object-property !oborel:has_participant))
	(declaration (object-property !inheres-in))
	(declaration (object-property !realizes))
	(annotation-assertion !rdfs:label !inheres-in "inheres in")
	(annotation-assertion !rdfs:label !oborel:has_participant "has participant")
	(annotation-assertion !rdfs:label !realizes "realizes")
	,@(owl-axioms-for-processes book))
    (write-rdfxml spreadsheet "ido:immunology;proto;pathway.owl")))


(defvar *immunology-uri-id-counter* 10000)

(defun fresh-immunology-uri ()
  (make-uri (format nil "http://purl.obolibrary.org/obo/IDO_~7,'0d" (incf *immunology-uri-id-counter*))))

(defmethod process-uri :around ((o parsed-process))
  (if (not (slot-boundp o 'process-uri)) 
      (setf (process-uri o) (fresh-immunology-uri))
      (or (call-next-method) (setf (process-uri o) (fresh-immunology-uri)))))

(defmethod all-participant-handles-defined? ((p parsed-process))
  (and (loop for e in (process-substrates p)
	  for handle = (lookup-handle (in-sheet (in-block p))  (second e))
	  always (and handle (handle-uri handle)))
       (loop for e in (process-products p)
	  for handle = (lookup-handle (in-sheet (in-block p))  (second e))
	  always (and handle (handle-uri handle)))))


(defun process-realizes-that-inheres-in (process realizable bearer)
  `(sub-class-of ,process
		 (object-some-values-from
		  !realizes
		  (object-intersection-of
		   ,realizable
		   (object-some-values-from !inheres-in ,bearer)))))

(defun has-participant-with-stoichiometry (process stoichiometry entity)
  (if (equal stoichiometry 1)
      `(sub-class-of ,process (object-some-values-from !oborel:has_participant ,entity))
      `(sub-class-of ,process (object-min-cardinality ,stoichiometry !oborel:has_participant ,entity))))

(defmethod owl-axioms ((p parsed-process))
  (let ((label 
	 (format nil "~{~a~^ + ~} -> ~{~a~^ + ~}"
		 (loop for p in (process-substrates p)
		    collect (if (equal (car p) 1) (second p) (format nil "~a ~a" (car p) (second p))))
		 (loop for p in (process-products p)
		    collect (if (equal (car p) 1) (second p) (format nil "~a ~a" (car p) (second p)))))))
    (if  (or (parse-errors p)
	     (not (all-participant-handles-defined?  p)))
	 (warn "Not generating OWL for ~a because there are parse errors or not all handles are defined" label)
	 (let ((uri (process-uri p)))
	   `((declaration (class ,uri))
	     (annotation-assertion !rdfs:label ,uri ,label)
	     ,@(loop for (stoichiometry handle) in (append (process-substrates p) (process-products p))
		  for entity = (and handle (lookup-handle (in-sheet (in-block p)) handle))
		  collect (has-participant-with-stoichiometry uri stoichiometry (handle-uri entity))
		  append (when (and (member handle (process-substrates p) :key 'second :test 'equal)
				    (not (member handle (process-products p) :key 'second :test 'equal)))
			   (list (process-realizes-that-inheres-in uri !substrate-disposition (handle-uri entity))))
		  append 
		    (when (and (not (member handle (process-substrates p) :key 'second :test 'equal))
			       (member handle (process-products p) :key 'second :test 'equal))
		      (list (process-realizes-that-inheres-in uri !product-disposition (handle-uri entity)))))
	     (sub-class-of ,uri !span:ProcessualEntity))))))


