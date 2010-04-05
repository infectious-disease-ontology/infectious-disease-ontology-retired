;; Syntax documentation: http://code.google.com/p/infectious-disease-ontology/wiki/Pathway_spreadsheet_syntax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move to utils

(when (not (fboundp 'mop::class-prototype))
  (let ((table (make-hash-table)))
    (defun mop::class-prototype (class)
      (or (gethash class table)
	  (setf (gethash class table) (make-instance class))))))

(defmethod class-slot-value ((class symbol) slot)
  (class-slot-value (find-class class)) slot)

(defmethod class-slot-value ((class standard-class) slot)
  (if (slot-boundp (mop::class-prototype class) slot)
      (slot-value (mop::class-prototype class) slot)
      (slot-value (initialize-instance (mop::class-prototype class)) slot)))

#+abcl (defmethod print-object :around ((o t) s)
  (multiple-value-bind (value errorp)
      (ignore-errors (call-next-method))
    (if errorp (print-unreadable-object (o s) (format s "error printing ~a" (type-of o))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Books

(defvar *ido-term-to-uri* (make-hash-table :test 'equalp))

(defclass ido-pathway-book (parsed-book)
  ((sheet-type :initform 'ido-pathway-sheet)))

(defmethod lookup-handle ((b ido-pathway-book) name)
  (let ((results 
	 (loop for sheet in (parsed-sheets b)
	      for found = (lookup-handle sheet name)
	      when found collect found)))
    (if (null (cdr results))
	(car results)
	(progn
	  (warn "handled defined more than once, ~s: ~s" name found)
	  results))))

(defmethod where-is-handle ((b ido-pathway-book) name)
  (loop for sheet in (parsed-sheets b)
     for found = (lookup-handle sheet name)
     when found collect sheet))

(defmethod parse-book ((b ido-pathway-book) &key (handles? t))
  (when handles? 
    (loop for sheet in (parsed-sheets b) do (clear-handles sheet)))
  (loop for block-type in (append (if handles? '(parsed-handle-block) nil)
				  '(parsed-sheet-info-block parsed-complex-block parsed-process-block))
     do
     (loop for block in (parsed-blocks b)
	when (typep block block-type)
	do (parse-block block)))
  (after-all-sheets-parsed b))

(defmethod after-all-sheets-parsed ((b ido-pathway-book))
  (classify-handles b)
  (verify-process-handles b)
  (parse-realizations b)
  (map nil 'after-all-sheets-parsed (parsed-sheets b))
  (map nil 'after-all-sheets-parsed (parsed-blocks b)))

(defmethod block-types ((book ido-pathway-book))
  (loop for class in (remove-duplicates (system:class-direct-subclasses (find-class 'ido-pathway-block)) :key 'class-name)
       collect (list* (class-name class) (class-slot-value class 'block-headers))))

(defmethod blocks-of-type  ((book ido-pathway-book) type)
  (remove type (parsed-blocks book) :test-not 'eq :key 'type-of))

(defmethod foreach-row-in-block-type ((book ido-pathway-book) type fn)
  (loop for block in (blocks-of-type book type)
       do (loop for raw-row in (block-rows block) for row in (parsed-rows block) do (funcall fn row))))

(defmethod write-external.owl ((book ido-pathway-book))
  (let ((axioms 
	 (loop for (kind where term parent) in *mireot-parent-terms*
	    unless (or (member where '("PRO" "PFAM") :test 'equal)
		       (null parent))
	    append
	    `((declaration (class ,term))
	      (subclassof ,term ,parent)
	      (annotationassertion !obo:IAO_0000412 ,term ,(make-uri (format nil "http://purl.org/obo/owl/~a" where)))))))
    (foreach-row-in-block-type 
     book 'parsed-handle-block 
     (lambda(e)
       (unless (or (not (uri-p (handle-uri e)))
		   (equal (handle-class e) "submitted"))
	 (multiple-value-bind (ont-uri ont-name) (term-ontology e)
	   (when (member ont-name '("CHEBI" "GO" "SO" "PATO") :test 'equal)
	     (setq axioms
		   (append 
		    `((declaration (class ,(handle-uri e)))
		      (subclassof ,(handle-uri e) ,(mireot-parent-term e))
		      (annotationassertion !obo:IAO_0000412 ,(handle-uri e) ,ont-uri))
		    axioms)))))))
    (map nil 'print axioms)
    (with-ontology external (:about !obo:ido/external.owl :base !obo: :eval t)
	axioms
      (write-rdfxml external "ido:immunology;external.owl"))))
    

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sheets

(defclass ido-pathway-sheet (parsed-sheet)
  ((lookup-table :accessor lookup-table :initarg :lookup-table :initform (make-hash-table :test 'equalp))
   (handle-lookup-sheets)
   (sheet-id :accessor sheet-id :initform "")
   ))

(defmethod after-all-sheets-parsed ((s ido-pathway-sheet))
  (handle-lookup-sheets s))

(defmethod handle-lookup-sheets ((s ido-pathway-sheet))
  (if (slot-boundp s 'handle-lookup-sheets )
      (slot-value s 'handle-lookup-sheets)
      (let* ((info-block (find 'parsed-sheet-info-block (parsed-blocks s) :key 'type-of))
	     (uses (and info-block
			(apply 'append (mapcar (lambda(e) (split-at-regex e "\\s*[,;]\\s*")) 
					       (remove nil (cdr (find "uses entities from" (block-rows info-block) :key 'car :test 'equalp))))))))
	(setf (sheet-id s) (and info-block (sheet-id info-block)))
	(setf (slot-value s 'handle-lookup-sheets)
	      (loop for used in uses
		 for sheet =  (or (find used (parsed-sheets (sheet-book s)) :key
					(lambda(s) (let ((info (find 'parsed-sheet-info-block (parsed-blocks s) :key 'type-of)))
						     (and info (sheet-id info))
						     ))
					:test 'equalp)
				  (find used (parsed-sheets (sheet-book s)) :key 'sheet-name :test 'equalp))
		 if (not sheet) do (push (format nil "Didn't find sheet ~a listed as uses entities from in ~a~%" used s) (parse-errors s))
		 else collect sheet)))))

(defmethod print-object ((o parsed-sheet) stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object (o stream :type t :identity nil)
      (format stream "~a[~a] in ~a - ~a blocks"
	      (sheet-name o)
	      (sheet-id o)
	      (book-path (sheet-book o))
	      (length (parsed-blocks o))))))
		   
(defmethod clear-handles ((s ido-pathway-sheet))
  (setf (lookup-table s) (make-hash-table :test 'equalp)))

(defmethod add-handle ((s ido-pathway-sheet) name object)
  (let ((existing (gethash name (lookup-table s))))
    (setq @ (list name object existing))
    (if (and existing
	     (eq (in-sheet (in-block existing)) (in-sheet (in-block object)))
	     (eql (in-row existing) (in-row object)))
	(setf (gethash name (lookup-table s)) object)
	(if existing
	    (let ((error (format nil "Duplicate handle ~s in ~a:~a,~a" name s object (gethash name (lookup-table s)))))
	      (push error (parse-errors object))
	      (push error (parse-errors (gethash name (lookup-table s)))))
	    (setf (gethash name (lookup-table s)) object)))))

(defmethod lookup-handle ((s ido-pathway-sheet) name)
  (or (gethash name (lookup-table s))
      (loop for sheet in (handle-lookup-sheets s)
	 for found = (gethash name (lookup-table sheet))
	 when found do (return-from lookup-handle found))))

(defmethod classify-handles ((book ido-pathway-book))
  (loop for block in (blocks-of-type book 'parsed-handle-block)
     with no-class with byprefix = (make-hash-table :test 'equal)
     do
       (loop
	  for row in (parsed-rows block) 
	  for class = (handle-class row)
	  if (null class)
	  do (push row no-class)
	  else do
	    (let ((simple (car (all-matches class "^(\\S+):(\\d+|submitted)$" 1 2))))
	      (if simple
		  (incf (gethash (car simple) byprefix 0))
		(if (#"matches" class "^(PFAM:){0,}PF\\d+$")
		    (incf (gethash "pfam" byprefix 0))
		    (unless (is-sole-product-of-reaction book (handle row))
		      (push (format nil "don't understand handle '~a' kind: '~a' id:'~a' description:'~a'"  (handle row) (handle-kind row)
			      class (handle-description row)) (parse-errors row))))))
       finally (return (values byprefix no-class)))))

(defmethod verify-process-handles ((book ido-pathway-book))
  (loop for block in (blocks-of-type book 'parsed-process-block)
     append
       (map nil 'verify-process-handles (parsed-rows block))))

(defmethod parse-realizations ((book ido-pathway-book))
  (loop for block in (blocks-of-type book 'parsed-process-block)
     append
       (map nil 'parse-realizations (parsed-rows block))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Blocks

(defclass ido-pathway-block (parsed-block))

(defmethod block-headers ((s symbol))
  (block-headers (mop::class-prototype (find-class s))))

(defclass parsed-handle-block (ido-pathway-block )
  ((block-headers :accessor block-headers :allocation :class :initform '("handle" "Entities" "Kind" "class" "super(s)"))
   (row-class :initform 'parsed-handle)
   ))

(defclass parsed-process-block (ido-pathway-block)
    ((block-headers :accessor block-headers :allocation :class :initform '("processes" "realizes" "part_of" "class" "super(s)" "binding domains"))
     (row-class :initform 'parsed-process)))

(defclass parsed-complex-block (ido-pathway-block)
    ((block-headers :accessor block-headers :allocation :class :initform '("complexes without process forming them" "has_part"))
     (row-class :initform 'parsed-complex)))

(defclass parsed-evidence-block (ido-pathway-block)
    ((block-headers :accessor block-headers :allocation :class :initform '("Evidence for" "Title" "Evidence Code" "Pubmed id"))
     (row-class :initform 'parsed-evidence)))

(defclass parsed-sheet-info-block (ido-pathway-block)
  ((block-headers :accessor block-headers :allocation :class :initform '("Spreadsheet id" "About" "Date Created" "Last edited" "Editors"))
   (sheet-id :accessor sheet-id :initform nil :initarg :sheet-id )
   (uses-handles-from :accessor uses-handles-from :initform nil :initarg :uses-handles-from)))

(defmethod parse-block ((i parsed-sheet-info-block))
  (let ((id (caar (block-rows i))))
;    (print-db i (in-sheet i) id)
    (when (null id)
      (setf (parse-errors id) (format nil "No id given for sheet ~a" i)))
    (setf (sheet-id i) id)))



;;(defmethod parse-block ((b parse-sheet-info-block))
;;  (let ((by-column (first (block-rows b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups of cells

(defclass ido-pathway-cells (parsed-cells))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handles

(defclass parsed-handle (ido-pathway-cells)
  ((handle :accessor handle :initarg :handle :initform nil)
   (handle-description :accessor handle-description :initarg :handle-description :initform nil)
   (handle-kind :accessor handle-kind :initarg :handle-kind :initform nil)
   (handle-class :accessor handle-class :initarg :handle-class :initform nil)
   (handle-super :accessor handle-super :initarg :handle-super :initform nil)
   (handle-uri :accessor handle-uri :initarg :handle-uri :initform nil)
  ))

(defmethod parse-row ((h parsed-handle))
  (let ((cell-list (cell-list h)))
    (setf (handle h) (first cell-list) (handle-description h) (second cell-list) 
	  (handle-kind h) (third cell-list) (handle-class h) (fourth cell-list)
	  (handle-super h) (fifth cell-list)))
  (add-handle (in-sheet (in-block h)) (handle h) h)
;  (if (handle-class h)
;      (if (#"matches" "^((GO:\\d+)|(PRO:\\d+)|$
  )

(defmethod print-summary ((o parsed-handle))
  (format nil "~s" (handle o)))

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

'(with-open-file (f "~/Downloads/2010-04-04/pfamA.txt")
	   (loop for line = (read-line f nil :eof)	
		repeat 5
		for fields = (mapcar (lambda(e) (#"replaceAll" e "(^')|('$)" "")) (split-at-char line #\tab))
	      do (print (cons (nth 4 fields) (nth 8 fields) ))))

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
		 (obolibrary-uri "PRO" id handle))
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
  (let ((name (caar (all-matches (uri-full (handle-uri o)) "(GO|SO|PATO|CHEBI)_" 1))))
    (and name
	 (return-from term-ontology (values (make-uri (format nil "http://purl.org/obo/owl/~a" name)) name))))
  (let ((name (caar (all-matches (uri-full (handle-uri o)) "(IDO|PRO|PFAM|OGMS)_" 1))))
    (and name
	 (values (make-uri (format nil "http://purl.obolibrary.org/obo/~a" name)) name))))

(defvar *mireot-parent-terms*
  '((:protein "PRO" !obo:PRO_000000001 !snap:MaterialEntity)
    (:molecular-function "GO" !oboont:GO#GO_0003674 !snap:Function)
    (:cellular-component "GO" !oboont:GO#GO_0005575 !snap:MaterialEntity)
    (:biological-process "GO" !oboont:GO#GO_0008150 !span:ProcessualEntity)
    (:molecular-entity "CHEBI" !oboont:CHEBI#CHEBI_23367 !snap:MaterialEntity)
    (:domain "PFAM" !obo:PRO_000018263 !snap:MaterialEntity)  ; amino acid chain. Should there be a term "domain"? SO:0000417 is POLYPEPTIDE_DOMAIN
    (:quality "PATO" !snap:Quality)
    (:protein-complex "GO" !oboont:GO#GO_0043234 !snap:MaterialEntity)))


(defmethod mireot-parent-term ((o parsed-handle))
  (multiple-value-bind (ont-uri ont-name) (term-ontology o)
    (cond ((member ont-name '("PRO" "CHEBI" "PFAM" "PATO") :test 'equal)
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
	   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Processes

(defclass parsed-process (ido-pathway-cells)
  ((process-def :accessor process-def :initarg :process-def :initform nil)
   (process-realizes :accessor process-realizes :initarg :process-realizes :initform nil)
   (process-part-of :accessor process-part-of :initarg :process-part-of :initform nil)
   (process-class :accessor process-class :initarg :process-class :initform nil)
   (process-supers :accessor process-supers :initarg :process-supers :initform nil)
   (binding-domains :accessor binding-domains :initarg :binding-domains :initform nil)
   (process-substrates :accessor process-substrates :initarg :process-substrates :initform nil)
   (process-products :accessor process-products :initarg :process-products :initform nil)))

(defmethod parse-row ((p parsed-process))
  ;; x + y + x -> a + b -> (:parsed|:notparsed list-of-substrates list-of-products)
  (let ((reaction-string (string-trim " " (car (cell-list p)))))
    (flet ((with-stoichiometry (piece)
	     (destructuring-bind (stoichiometry entity) (car (all-matches piece "((\\d+\\s+){0,1})([A-Za-z0-9-]+)" 1 3))
	       (list (if (equal stoichiometry "") 1 (parse-integer stoichiometry )) entity))))
      (cond ((#"matches" reaction-string "^(((\\d+\\s+){0,1}[A-Za-z0-9-]+?)\\s*\\+\\s*)*((\\d+\\s+){0,1}[A-Za-z0-9-]+?)\\s*(->)\\s*(((\\d+\\s+){0,1}[A-Za-z0-9-]+?)\\s*\\+\\s*)*((\\d+\\s+){0,1}[A-Za-z0-9-]+?)\\s*$")
	     (let ((parsed
		    (mapcar (lambda(e) (split-at-regex e "\\s*\\+\\s*"))
			    (split-at-regex reaction-string "\\s*->\\s*"))))
	       (setf (process-substrates p) (mapcar #'with-stoichiometry (first parsed))
		     (process-products p) (mapcar #'with-stoichiometry (second parsed)))))
	    (t (setf (parse-errors p) (list "didn't match expected form")))))))

(defmethod parse-realizations ((p parsed-process))
  (let ((string (second (cell-list p))))
    (and string
	 (let ((realizations (split-at-regex string "\\s+(;|(and))\\s+")))
	   (loop for r in realizations
	      for matches = (all-matches r "^\\s*([A-Za-z0-9-]+)\\s+of\\s+(\\S+)(\\s+((part of)|(in)|)\\s+([A-Za-z0-9-]+)){0,1}\\s*$"
					 1 2 7 )
	      do (if (null matches)
		     (push (format nil "Don't recognize form of realizations expression: '~a'" r) (parse-errors p))
		     (progn
		       (push matches (process-realizes p))
		       (loop for match in matches
			    do
			    (loop for element in match
			       when (and element (not (lookup-handle (in-sheet (in-block p)) element)))
			       do
				 (push (format nil "Didn't find handle for realization element '~a' in '~a'" element r) (parse-errors p))))
		     )))))))

(defmethod verify-process-handles ((p parsed-process))
  (loop for (nil handle) in (append (process-substrates p) (process-products p))
       for found = (lookup-handle (in-sheet (in-block p))  handle)
       unless (or found (equal handle "0")) do
       (let ((somewhere (where-is-handle (sheet-book (in-sheet (in-block p))) handle)))
	 (format t "Didn't find handle ~a from ~a~a~%" handle p
		 (if somewhere
		     (format nil " did you mean to use handles from ~{~a~^ or ~}" somewhere)
		     "")
		 ))))

(defmethod print-summary ((o parsed-process))
  (format nil "~s" (car (cell-list o))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complexes

(defclass parsed-complex (ido-pathway-cells)
  ((handle :accessor handle :initarg :handle :initform nil)
   (complex-elements :accessor complex-elements :initarg :complex-elements :initform nil)
   (complex-definition-type :accessor complex-definition-type :initarg :complex-definition-type :initform nil)))

(defmethod parse-row ((o parsed-complex))
  (let ((name (first (cell-list o)))
	(parts (second (cell-list o))))
    ;; name, parts -> (:parsed|:notparsed (name) (:assembly|:union list-of-products))
    (unless (and (stringp name) (stringp parts))
      (when (not (stringp name))
	(push (format nil "~s is not a string" name) (parse-errors o)))
      (when (not (stringp parts))
	(push (format nil "~s is not a string" parts) (parse-errors o)))
      (return-from parse-row nil))
    (setq name (string-trim " " name))
    (setf (handle o) name)
    (setq parts (string-trim " " parts))
    (unless (#"matches" name "^[A-Za-z0-9-]+$")
      (push (format nil "~s isn't a valid name for a complex" name) (parse-errors o))
      (return-from parse-row nil))
    (unless (#"matches" parts "^(([A-Za-z0-9-]+?)\\s*([+,]|(\\bor\\b))\\s*)*([A-Za-z0-9-]+?)\\s*$")
      (push (format nil "~s isn't formated as parts of a complex" parts) (parse-errors o))
      (return-from parse-row nil))
    (setf (complex-definition-type o)
	  (if (#"matches" parts ".*\\bor\\b.*")
	      :union :assembly))
    (setf (complex-elements o) (split-at-regex parts "\\s*([+,]|(\\bor\\b))\\s*"))))

(defmethod print-summary ((o parsed-complex))
  (or (handle o) "parse error"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evidence

(defclass parsed-evidence (ido-pathway-cells)
  )

(defmethod print-summary ((o parsed-evidence))
  "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging code

(defun check-processes-use-only-defined-handles (complexes handles processes)
  (loop for (process) in processes
       for parsed = (parse-process process)
       when (eq (car parsed) :notparsed) do (print parsed)
       when (eq (car parsed) :parsed)
       do
       (print parsed)
       (loop for term in (append (second parsed) (third parsed))
	    when (not (or (find term handles :key 'car :test 'equalp) (find term complexes :key 'car :test 'equalp)))
	    do (format t "~%Did't find ~s from ~s~%" term process)))
  (loop for (complex-name complex-parts) in complexes
       for parsed = (parse-complex complex-name complex-parts)
       when (eq (car parsed) :notparsed) do (print parsed)
       when (eq (car parsed) :parsed)
       do
       (print parsed)
       (loop for term in (cdr (third parsed))
	    when (not (or (find term handles :key 'car :test 'equalp) (find term complexes :key 'car :test 'equalp)))
	    do (format t "~%Did't find ~s from ~s~%" term (append (second parsed) (third parsed)))))
  )

(defun check-pathway-spreadsheet-syntax (&key (path "ido:immunology;ido-s4lps-tlr4.xlsx"))
  (let* ((book (make-instance 'ido-pathway-book :book-path path)))
    (locate-blocks-in-sheets book (mapcar 'car (block-types book)))
    (parse-book book)
    (report-some-processes book)
    (report-parse-results book)
    book))

;(test)

(defun report-some-processes (book)
  (format t "Processes that pass first level syntax check~%")
  (loop for bl in (blocks-of-type book 'parsed-process-block)
     do 
     (loop for p in (parsed-rows bl)
	when (not (parse-errors p)) do (format t "~{~a~^ + ~} -> ~{~a~^ + ~}~%"
					       (loop for p in (process-substrates p)
						  collect (if (equal (car p) 1) (second p) (format nil "~a ~a" (car p) (second p))))
					       (loop for p in (process-products p)
						  collect (if (equal (car p) 1) (second p) (format nil "~a ~a" (car p) (second p))))))))

(defun report-handles (book)
  (loop for block in (blocks-of-type book 'parsed-handle-block)
       with kinds
     do
       (loop for  row in (parsed-rows block) 
	  do (format t "~a	~a	~a	~a	~a~%"
		     (sheet-name (in-sheet block))
		     (handle row)
		     (handle-kind row)
		     (handle-class row)
		     (handle-description row))
	    (pushnew (handle-kind row) kinds :test 'equalp))
       finally (format t "Kinds: ~{~a~^~% ~}" kinds)))


(defun is-sole-product-of-reaction (book handle)
  (loop for block in (blocks-of-type book 'parsed-process-block)
     thereis
     (loop for process in (parsed-rows block)
	for products = (process-products process)
	thereis (and (= (length products) 1) (equalp (second (car products)) handle)))))

(defun processes-mentioning-handle (book handle)
  (loop for block in (blocks-of-type book 'parsed-process-block)
     thereis
     (loop for process in (parsed-rows block)
	for products = (process-products process)
	  when (or (find handle (process-substrates process) :key 'second :test 'equalp)
		   (find handle (process-products process) :key 'second :test 'equalp))
	  do (print process))))
				    

	      