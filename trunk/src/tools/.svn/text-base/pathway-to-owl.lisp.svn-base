(defvar *ido-term-to-uri* (make-hash-table :test 'equalp))

(defmethod handle-uri :around ((o parsed-handle))
  (or (call-next-method) (setf (handle-uri o) (compute-handle-uri o))))

(defun legacy-uri (prefix id handle)
  (cond ((#"matches" id "^\\d+$")
         (make-uri (format nil "http://purl.org/obo/owl/~a#~a_~a" prefix prefix id)))
	((or (equal id "submitted") (equal id "tosubmit"))
	 (make-uri (make-uri (format nil "http://purl.org/obo/owl/~a#submitted_~a" prefix (string-downcase handle)))))
	(t (error "Don't know how to make uri for ~a:~a (~a)" prefix id handle))))

(defun obolibrary-uri (prefix id handle)
  (cond ((#"matches" id "^\\d+$")
         (make-uri (format nil "http://purl.obolibrary.org/obo/~a_~a" prefix id)))
	((or (equal id "submitted") (equal id "tosubmit"))
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
		   ((or (equal id "submitted") (equal id "tosubmit"))
		    (make-uri (format nil "http://purl.obolibrary.org/obo/~a_submitted_~a" prefix (string-downcase handle))))
		   (t (error "compute-uri base ~a prefix ~a id ~a handle ~a" base prefix id handle)))))
      (unless (null id)
	(destructuring-bind (prefix id) (or (car (all-matches id "(.*):(.*)" 1 2))
					    (car (all-matches id "(PF)(.*)" 1 2))) ; handle PF without "PFAM" prefix.
	  (cond ((equal prefix "GO")
		 (assert (or (#"matches" id "^\\d+") (equal id "submitted")  (equal id "tosubmit")) (id) "GO id GO:~a malformed" id)
		 (legacy-uri "GO" id handle)) 
		((equal prefix "PRO")
		 (legacy-uri "PRO" id handle))
		((equal prefix "MI")
					;("compr" "competitor" "role" "MI:0941" NIL) 
					;("compb" "competition binding" "process" "MI:0405" NIL) 
		 ;; not used
		 (format t "warning: Not translating MI term MI:~a~%" id)
		 )
		((equal prefix "PMID") (format t  "warning: ~a ~a~%" handle id))
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
		((equal prefix "MGI")
		 (legacy-uri "MGI" id handle)
		 (warn "MGI id!")
		 )
		(t (error "compute-handle-uri prefix ~a" prefix))))))))

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
    (cond ((member ont-name '("CHEBI" "PFAM" "PATO" "SO") :test 'equal)
	   (third (find ont-name *mireot-parent-terms* :test 'equal :key 'second)))
	  ((equal ont-name "PRO")
	   (let ((type (handle-kind o)))
	     (cond ((search "complex" type :test #'char=)
		    (third (assoc :protein-complex *mireot-parent-terms*)))
		   ((member type '("protein" "gene product") :test 'equalp)
		    (third (assoc :protein *mireot-parent-terms*)))
		   ((equal type "post transcriptional modification")
		    (if (or (find #\- (handle o)) (find #\+ (handle-description o)) (>= (length (handle o)) 12))
			(progn
			  (format t "for ~a annotated ptm assuming complex~%" o)
			  (third (assoc :protein-complex *mireot-parent-terms*)))
			(progn
			  (format t "for ~a annotated ptm assuming protein~%" o)
			  (third (assoc :protein *mireot-parent-terms*)))))
		   (t (error "don't know what ~a as type means in ~a" type o)))))
	  ((equal ont-name "GO")
	   (let ((type (handle-kind o)))
	     (cond ((member type '("function" "molecular function") :test 'equalp)
		    (third (assoc :molecular-function *mireot-parent-terms*)))
		   ((member type '("process" "biological process") :test 'equalp)
		    (third (assoc :biological-process *mireot-parent-terms*)))
		   ((member type '("CC" "GO CC" "cellular component") :test 'equalp)
		    (if (or (find #\+ (handle-description o)) (search "complex" (handle-description o)))
			(progn
			  (format t "assuming ~a is complex rather than only cellular component~%" o)
			  (third (assoc :protein-complex *mireot-parent-terms*)))
			(third (assoc :cellular-component *mireot-parent-terms*))))
		   ((equalp type "complex")
		    (third (assoc :protein-complex *mireot-parent-terms*)))
		   )))
	  (t (error "Don't know parent for ~a" o)))))

(defmethod entity-symbolic-type ((o parsed-handle))
  (car (find (mireot-parent-term o) *mireot-parent-terms* :key 'third)))


(defmethod spreadsheet-source-editor-note ((e parsed-handle))
  `(annotation-assertion !obo:IAO_0000116 ,(handle-uri e)
			 ,(format nil "handle ~a from row ~a of ~a in sheet ~a from workbook ~a"
				  (handle e)
				  (in-row e)
				  (string-downcase (string (type-of (in-block e))))
				  (sheet-name (in-sheet (in-block e)))
				  (pathname-name (book-path (sheet-book (in-sheet (in-block e))))))))

(defmethod spreadsheet-source-editor-note ((e parsed-process))
  `(annotation-assertion !obo:IAO_0000116 ,(process-uri e)
			 ,(format nil "process ~{~a~^, ~} from row ~a of ~a in sheet ~a from workbook ~a"
				  (mapcar (lambda(e) (or e "")) (cell-list e))
				  (in-row e)
				  (string-downcase (string (type-of (in-block e))))
				  (sheet-name (in-sheet (in-block e)))
				  (pathname-name (book-path (sheet-book (in-sheet (in-block e))))))))

(defmethod write-external.owl ((book ido-pathway-book) &key (destdir "ido:immunology;proto;"))
  (let ((axioms 
	 (loop for (kind where term parent) in (eval-uri-reader-macro *mireot-parent-terms*)
	    unless (or (member where '("PFAM") :test 'equal)
		       (null parent))
	    append
	      `((declaration (class ,term))
	      (subclassof ,term ,parent)
		(annotationassertion ,!obo:IAO_0000412 ,term ,(make-uri (format nil "http://purl.org/obo/owl/~a" where)))
	      ))))
    (foreach-row-in-block-type 
     book 'parsed-handle-block 
     (lambda(e)
       (unless (not (uri-p (handle-uri e)))
	 (multiple-value-bind (ont-uri ont-name) (term-ontology e)
	   (when (member ont-name '("CHEBI" "GO" "SO" "PATO" "PRO") :test 'equal)
	     (setq axioms
		   (append 
		    `((declaration (class ,(handle-uri e)))
		      (sub-class-of ,(handle-uri e) ,(mireot-parent-term e))
		      (annotation-assertion !obo:IAO_0000412 ,(handle-uri e) ,ont-uri)
		      ,(spreadsheet-source-editor-note e)
		      )
		    (if (#"matches" (handle-class e) ".*(submitted|tosubmit).*")
			`((annotation-assertion ,!rdfs:label ,(handle-uri e) ,(handle-description e)))
			)
		    axioms)
		   ))))))
    (let ((ont-uri (make-uri (format nil "obo:ido/dev/~a-pathway-external.owl" (#"replaceAll" (pathway-name book) "\\s+" "-")))))
      (with-ontology external (:about ont-uri :base !obo: :collecting t)
	  ((asq (imports !obo:iao/ontology-metadata.owl))
	   (as axioms))
	(write-rdfxml external (merge-pathnames (format nil "~a-pathway-external.owl" (#"replaceAll" (pathway-name book) "\\s+" "-")) destdir))))))

;; doesn't yet run in owlapi3 - run in older

(defmethod write-external-derived.owl ((book ido-pathway-book))
  (let ((name (#"replaceAll" (pathway-name book) "\\s+" "-")))
    (create-external-derived :kb (load-ontology (namestring (truename (format nil "ido:immunology;proto;~a-pathway-external.owl" name))))
			     :output-path (format nil "ido:immunology;proto;~a-pathway-external-derived.owl" name)
			     :templates-path "ido:tools;immunology-external-templates.txt")))

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
(def-uri-alias "occurs-in" !<http://purl.org/obo/owl/OBO_REL#occurs_in>)
(def-uri-alias "has-output" !<http://purl.org/obo/owl/OBO_REL#has-output>)
(def-uri-alias "has-participant" !oborel:has_participant)
(def-uri-alias "biological-process" !oboont:GO#GO_0008150)

(defmethod write-pathway.owl ((book ido-pathway-book) &key (destdir "ido:immunology;proto;"))
  (let ((ont-uri (make-uri nil (format nil "obo:ido/dev/~a-pathway.owl" (#"replaceAll" (pathway-name book) "\\s+" "-")))))
    (with-ontology spreadsheet (:about ont-uri :eval t)
	`((imports ,(make-uri nil (format nil "obo:ido/dev/~a-pathway-external.owl" (#"replaceAll" (pathway-name book) "\\s+" "-"))))
	  (imports !obo:ido/dev/pathway-defs.owl)
	  (imports ,(make-uri nil (format nil "obo:ido/dev/~a-pathway-external-derived.owl" (#"replaceAll" (pathway-name book) "\\s+" "-"))))
	  (imports !bfo:)
	  (imports !obo:iao/ontology-metadata.owl)
	  (imports !<http://www.obofoundry.org/ro/ro.owl>)
	  (declaration (object-property !has-participant))
	  (declaration (object-property !occurs-in))
	  (declaration (annotation !rdfs:label  "inheres in") (object-property !inheres-in))
	  (declaration (annotation !rdfs:label  "occurs in") (object-property !occurs-in))
	  (declaration (object-property !realizes))
	  (sub-object-property-of !has-output !has-participant)
	  (annotation-assertion !rdfs:label !inheres-in "inheres in")
	  (annotation-assertion !rdfs:label !oborel:has_participant "has participant")
	  (annotation-assertion !rdfs:label !realizes "realizes")
	  ,@(owl-axioms-for-processes book))
      (write-rdfxml spreadsheet (merge-pathnames  (format nil "~a-pathway.owl" (#"replaceAll" (pathway-name book) "\\s+" "-")) destdir)))))

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

(defun process-realizes-that-inheres-in-axiom (process realizable bearer)
  `(sub-class-of ,process
		 (object-some-values-from
		  !realizes
		  (object-intersection-of
		   ,realizable
		   (object-some-values-from !inheres-in ,bearer)))))

(defun has-participant-with-stoichiometry-axiom (process stoichiometry entity &optional product?)
  (if (equal stoichiometry 1)
      `(sub-class-of ,process (object-some-values-from ,(if product? !has-output !oborel:has_participant) ,entity))
      `(sub-class-of ,process (object-exact-cardinality ,stoichiometry ,(if product? !has-output !oborel:has_participant) ,entity))))

(defmethod only-has-protein-participants-axiom ((p parsed-process))
  `(sub-class-of
    ,(process-uri p)
    (object-all-values-from
     !oborel:has_participant
     (object-union-of 
      ,@(mapcar (lambda(e) (handle-uri (lookup-handle p e))) (union (mapcar 'second (process-substrates p))
								    (mapcar 'second (process-products p))))
      (object-complement-of (object-union-of !oboont:PRO#PRO_000000001 !oboont:GO#GO_0043234 ))))))

(defmethod process-curated-realizations-axioms ((p parsed-process))
  (if (loop for realizes in (process-realizes p) always
	   (loop for handle in realizes
	      always (or (null handle)
			 (and (lookup-handle p handle)
			      (handle-uri (lookup-handle p handle))))))
      (loop for realizes in (process-realizes p)
	   collect
	   (destructuring-bind (realizable bearer bearer-whole) realizes
	     (process-realizes-that-inheres-in-axiom
	      (process-uri p) (handle-uri (lookup-handle p realizable))
	      (if bearer-whole 
		  `(object-intersection-of
		    ,(handle-uri (lookup-handle p bearer))
		    (object-some-values-from !oborel:part_of ,(handle-uri (lookup-handle p bearer-whole))))
		  (handle-uri (lookup-handle p bearer))))))
      (format t  "warning: Not generating OWL for realizations for ~a because there are parse errors or not all handles are defined~%" p)))

(defmethod process-part-located-in-axiom ((p parsed-process))
  (destructuring-bind (larger-process location) (process-part-of p)
    (let ((larger-process-uri (and (lookup-handle p larger-process) (handle-uri (lookup-handle p larger-process))))
	  (location-uri (and (lookup-handle p location) (handle-uri (lookup-handle p location)))))
      (append
       (when larger-process-uri
	 `((sub-class-of ,(process-uri p) (object-some-values-from !oborel:part_of ,larger-process-uri))))
       (when location-uri 
	 `((sub-class-of ,(process-uri p) (object-some-values-from !roproposed:occurs_in ,location-uri))))))))

(defmethod owl-axioms ((p parsed-process))
  (let ((label 
	 (format nil "~{~a~^ + ~} -> ~{~a~^ + ~}~a"
		 (loop for p in (process-substrates p)
		    collect (if (equal (car p) 1) (second p) (format nil "~a ~a" (car p) (second p))))
		 (loop for p in (process-products p)
		    collect (if (equal (car p) 1) (second p) (format nil "~a ~a" (car p) (second p))))
		 (if (lookup-handle p (second (process-part-of p)))
		     (format nil " in ~a" (second (process-part-of p)))
		     "")
		 )))
    (if  (or (parse-errors p)
	     (not (all-participant-handles-defined?  p)))
	 (format t  "warning Not generating OWL for ~a because there are parse errors or not all handles are defined~%" label)
	 (let ((uri (process-uri p)))
	   `((declaration (class ,uri))
	     (annotation-assertion !rdfs:label ,uri ,label)
	     ,@(loop for (stoichiometry handle) in (process-substrates p)
		  for entity = (and handle (lookup-handle (in-sheet (in-block p)) handle))
		  collect (has-participant-with-stoichiometry-axiom uri stoichiometry (handle-uri entity))
		  append (list (process-realizes-that-inheres-in-axiom uri !substrate-disposition (handle-uri entity))))
	     ,@(loop for (stoichiometry handle) in (process-products p)
		  for entity = (and handle (lookup-handle (in-sheet (in-block p)) handle))
		  collect (has-participant-with-stoichiometry-axiom uri stoichiometry (handle-uri entity) t))
	     ,@(process-curated-realizations-axioms p)
	     ,@(process-part-located-in-axiom p)
	     ,(only-has-protein-participants-axiom p)
	     ,(spreadsheet-source-editor-note p)
	     (sub-class-of ,uri !biological-process))))))

;~/Downloads/2010-05-26/pro_wv.obo


;;; decision: Aggregates of (each type of) molecule. Aggregates of processes (each tab is a pathway). The aggregate continuants are participants of the aggregate processes. The GO processes are at the aggregate levels, e.g. the SARM+ reaction is part of a GO negative regulation of..
;; is_component_process_of
;; is_grain_of 