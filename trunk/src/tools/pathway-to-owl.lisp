;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move to utils

(when (not (fboundp 'mop::class-prototype))
  (let ((table (make-hash-table)))
    (defun mop::class-prototype (class)
      (or (gethash class table)
	  (setf (gethash class table) (allocate-instance class))))))

(defmethod class-slot-value ((class symbol) slot)
  (slot-value (mop::class-prototype (find-class class)) slot))

(defmethod class-slot-value ((class standard-class) slot)
  (slot-value (mop::class-prototype class) slot))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido specific

(defvar *ido-term-to-uri* (make-hash-table :test 'equalp))

(defparameter *blocks*
  '((:handles "handle" "Entities" "Kind" "class"	"super(s)")
    (:processes "processes" "realizes" "part_of" "class" "super(s)" "binding domains")
    (:complexes "complexes without process forming them" "has_part")
    (:evidence "Evidence for"	"Title"	"Evidence Code"	"Pubmed id")
    ))

(defclass ido-pathway-book (parsed-book))

(defmethod block-types ((book ido-pathway-book))
  (loop for class in (system:class-direct-subclasses (find-class 'ido-pathway-block))
       collect (list* (class-name class) (class-slot-value class 'block-headers))))

(defclass ido-pathway-block (parsed-block))

(defclass ido-pathway-cell (parsed-cell))

(defmethod block-headers ((s symbol))
  (block-headers (mop::class-prototype (find-class s))))

(defclass parsed-handle-block (ido-pathway-block )
  ((block-headers :accessor block-headers :allocation :class :initform '("handle" "Entities" "Kind" "class" "super(s)"))))

(defclass parsed-process-block (ido-pathway-block)
    ((block-headers :accessor block-headers :allocation :class :initform '("processes" "realizes" "part_of" "class" "super(s)" "binding domains"))))

(defclass parsed-complex-block (ido-pathway-block)
    ((block-headers :accessor block-headers :allocation :class :initform '("complexes without process forming them" "has_part"))))

(defclass parsed-evidence-block (ido-pathway-block)
    ((block-headers :accessor block-headers :allocation :class :initform '("Evidence for" "Title" "Evidence Code" "Pubmed id"))))

(defun make-ido-pathway-block (block-type &rest args)
  (apply 'make-instance (ecase block-type
			  (:handles 'parsed-handle-block)
			  (:processes 'parsed-process-block)
			  (:complexes 'parsed-complex-block)
			  (:evidence 'parsed-evidence-block))
	 args))

(defclass parsed-handle (parsed-cells)
  ((handle :accessor handle :initarg :handle :initform nil)
   (handle-description :accessor handle-description :initarg :handle-description :initform nil)
   (handle-kind :accessor handle-kind :initarg :handle-kind :initform nil)
   (handle-class :accessor handle-class :initarg :handle-class :initform nil)
   (handle-super :accessor handle-super :initarg :handle-super :initform nil))
  )

(defmethod print-summary ((o parsed-handle))
  (handle o))

(defclass parsed-process (parsed-cells)
  ((process-def :accessor process-def :initarg :process-def :initform nil)
   (process-realizes :accessor process-realizes :initarg :process-realizes :initform nil)
   (process-part-of :accessor process-part-of :initarg :process-part-of :initform nil)
   (process-class :accessor process-class :initarg :process-class :initform nil)
   (process-supers :accessor process-supers :initarg :process-supers :initform nil)
   (binding-domains :accessor binding-domains :initarg :binding-domains :initform nil)))

(defmethod print-summary ((o parsed-process))
  (process-def o))

(defclass parsed-complex (parsed-cells)
  ((handle :accessor handle :initarg :handle :initform nil)
   (complex-parts :accessor complex-parts :initarg :complex-parts :initform nil))
  )

(defmethod print-summary ((o parsed-complex))
  (handle o))

(defun translate-pathway ()
  (get-sheets :file "ido:immunology;ido-s4lps-tlr4.xls"))


;(locate-blocks-in-sheets "ido:immunology;ido-s4lps-tlr4.xls" '(:handles :processes) '("TLR4MyD88"))

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

(defun parse-process (string)
  ;; x + y + x -> a + b -> (:parsed|:notparsed list-of-substrates list-of-products)
  (setq string (string-trim " " string))
  (cond ((#"matches" string "^(([A-Za-z0-9-]+?)\\s*\\+\\s*)*([A-Za-z0-9-]+?)\\s*(->)\\s*(([A-Za-z0-9-]+?)\\s*\\+\\s*)*([A-Za-z0-9-]+?)\\s*$")
	 (list* :parsed (mapcar (lambda(e) (split-at-regex e "\\s*\\+\\s*"))
			       (split-at-regex string "\\s*->\\s*"))))
	(t (list :notparsed string))))

(defun parse-complex (name parts)
  ;; name, parts -> (:parsed|:notparsed (name) (:assembly|:union list-of-products))
  (unless (and (stringp name) (stringp parts))
    (return-from parse-complex (list :notparsed (list name) (list parts) :notstrings)))
  (setq name (string-trim " " name))
  (setq parts (string-trim " " parts))
  (unless (#"matches" name "^[A-Za-z0-9-]+$")
    (return-from parse-complex (list :notparsed (list name) (list parts) :bad-name)))
  (unless (#"matches" parts "^(([A-Za-z0-9-]+?)\\s*([+,]|(\\bor\\b))\\s*)*([A-Za-z0-9-]+?)\\s*$")
    (return-from parse-complex (list :notparsed (list name) (list parts) :bad-parts)))
  (list :parsed (list name) (list* (if (#"matches" parts ".*\\bor\\b.*") :union :assembly) (split-at-regex parts "\\s*([+,]|(\\bor\\b))\\s*"))))

(defun test ()
  (let* ((book (make-instance 'ido-pathway-book :book-path "ido:immunology;ido-s4lps-tlr4.xlsx"))
	 (found (locate-blocks-in-sheets book (mapcar 'car (block-types book))))
	 (handles (mapcan 'block-rows (remove 'parsed-handle-block found :key 'type-of :test-not 'eq)))
	 (processes (mapcan 'block-rows (remove 'parsed-process-block found :key 'type-of :test-not 'eq)))
	 (complexes (mapcan 'block-rows (remove 'parsed-complex-block found :key 'type-of :test-not 'eq))))
    (print-db processes)
    (print-db complexes)
    (check-processes-use-only-defined-handles complexes handles processes)
    book))



;; (:NOTPARSED "rlps-tmIRK4IRK1->rlps-tmIRK4IRK1p   (Thr 387)") 
;; (:NOTPARSED "tak1 part of IRK1pptf6pubpelptak1tab1tab2tab3 posphorilates IKbK part of IRK1ubpptf6pubpelpIKbK-> IRK1ubpptf6pubpelpIKbKp") 
;; (:NOTPARSED "IKBNfkb") 
;; (:NOTPARSED "IKbKp part of IRK1ubpptf6pubpelpIKbKp posphorilates IKB part of IKBNfkb-> IKBpNfkb") 
;; (:NOTPARSED "IKBpK48ub->IKBpK48ub degraded also as participant in GO:0006511") 
;; (:NOTPARSED "Nfkb translocate to nucleus") 
;; (:NOTPARSED "nfkb") 
;; (:NOTPARSED "Nfkb this will lead to a cytokine and chemokine production                               T realizes transcripton factor activity leading to transcription and translation to protein K") 
