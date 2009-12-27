(defvar *ido-term-to-uri* (make-hash-table :test 'equalp))

(defclass parsed-sheet ()
  ((sheet-book :accessor sheet-book :initarg :sheet-book :initform nil)
   (sheet-name :accessor sheet-name :initarg :sheet-name :initform nil) 
   (parsed-blocks :accessor parsed-blocks :initarg :parsed-blocks :initform nil)
   (sheet-rows :accessor sheet-rows :initarg :sheet-rows :initform nil)
   (java-sheet :accessor java-sheet :initarg :java-sheet :initform nil)))

(defmethod print-object ((o parsed-sheet) stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object (o stream :type t :identity nil)
      (format stream "~a in ~a - ~a blocks"
	      (sheet-name o)
	      (book-path (sheet-book o))
	      (length (parsed-blocks o))))))

(defclass parsed-book ()
  ((book-path :accessor book-path :initarg :book-path :initform nil)
   (parsed-sheets :accessor parsed-sheets :initarg :parsed-sheets :initform nil)
   (parsed-blocks :accessor parsed-blocks :initarg :parsed-blocks :initform nil)))

(defmethod print-object ((o parsed-book) stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object (o stream :type t :identity nil)
      (format stream "~s - ~a sheets, ~a blocks"
	      (book-path o)
	      (length (parsed-sheets o))
	      (length (parsed-blocks o))))))

(defclass parsed-cells ()
  ((in-sheet :accessor in-sheet :initarg :in-sheet :initform nil)
   (first-row :accessor first-row :initarg :first-row :initform nil)
   (start-cell :accessor start-cell :initarg :start-cell :initform nil)
   (end-cell :accessor end-cell :initarg :end-cell :initform nil)))

(defmethod print-summary ((o parsed-cells))
  "")

(defmethod print-object ((object parsed-cells)  stream)
  (let ((*print-case* :downcase))
    (print-unreadable-object (object stream :type t :identity nil)
      (format stream "~a:[~a-~a]@~a ~a"
	      (sheet-name (in-sheet object)) (start-cell object) (end-cell object) (first-row object)
	      (print-summary object)))))

(defclass parsed-block (parsed-cells)
  ((block-type :accessor block-type :initarg :block-type :initform nil)
   (block-rows :accessor block-rows :initarg :block-rows :initform nil)))

(defmethod print-summary ((o parsed-block))
  (format nil "~a ~a rows" (string-downcase (string (block-type o))) (length (block-rows o))))

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

(defparameter *blocks*
  '((:handles "handle" "Entities" "Kind" "class"	"super(s)")
    (:processes "processes" "realizes" "part_of" "class" "super(s)" "binding domains")
    (:complexes "complexes without process forming them" "has_part")
    (:evidence "Evidence for"	"Title"	"Evidence Code"	"Pubmed id")
    ))

(defun find-block (block-type sheet)
  (loop for row in (sheet-rows sheet)
     for rowcount from 1 with found-row
     with collecting
     with headers = (cdr (assoc block-type *blocks*))
     for found = (or found (search headers row :test 'equalp))
					;     do (print-db found collecting row)
     when (and (eq collecting :found) (not (null (nth found row)))) do (setq collecting :collect)
     when (and found (null collecting))
     do (progn (setq collecting :found)
	       (setq found-row rowcount))
     until (and (eq collecting :collect)
		(or (null (nth found row))
		    (some (lambda(h) (search (cdr h) row :test 'equalp)) *blocks*))
		)
     when (eq collecting :collect) collect
     (mapcar (lambda(e)
	       (if (stringp e) (string-trim " " e) e))
	     (subseq row found (+ found (length headers))))
     into rows
     finally
     (return-from find-block
       (and found (make-instance 'parsed-block :in-sheet sheet :first-row found-row :start-cell found :end-cell (1- (+ found (length headers)))
				 :block-rows rows :block-type block-type )))))

(defun locate-blocks-in-sheets (book types &optional within-sheets)
  (setf (parsed-blocks book)
	(loop
	   for (sheet-name sheet) in (list-sheets :file (book-path book))
	   for parsed-sheet =
	   (or (find sheet-name (parsed-sheets book) :test 'equalp :key 'sheet-name)
	       (let ((new 
		      (make-instance 'parsed-sheet :sheet-rows (get-sheet-as-row-lists sheet) :sheet-book book :sheet-name sheet-name
				     :java-sheet sheet)))
		 (push new (parsed-sheets book))
		 new))
	   for found = (and (or (null within-sheets) (member sheet-name within-sheets :test 'equalp))
			    (loop for type in types
			       for found = (find-block type parsed-sheet)
			       when found collect found))
	   do (setf (parsed-blocks parsed-sheet) found)
	   append found 
	   )))

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
  (let* ((book (make-instance 'parsed-book :book-path "ido:immunology;ido-s4lps-tlr4.xlsx"))
	 (found (locate-blocks-in-sheets book '(:handles :processes :complexes)))
	 (handles (mapcan 'block-rows (remove :handles found :key 'block-type :test-not 'eq)))
	 (processes (mapcan 'block-rows (remove :processes found :key 'block-type :test-not 'eq)))
	 (complexes (mapcan 'block-rows (remove :complexes found :key 'block-type :test-not 'eq))))
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
