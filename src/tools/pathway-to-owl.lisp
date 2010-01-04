;; Syntax documentation: http://code.google.com/p/infectious-disease-ontology/wiki/Pathway_spreadsheet_syntax

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move to utils

(when (not (fboundp 'mop::class-prototype))
  (let ((table (make-hash-table)))
    (defun mop::class-prototype (class)
      (or (gethash class table)
	  (setf (gethash class table) (make-instance class))))))

(defmethod class-slot-value ((class symbol) slot)
  (slot-value (mop::class-prototype (find-class class)) slot))

(defmethod class-slot-value ((class standard-class) slot)
  (slot-value (mop::class-prototype class) slot))

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
	      for found = (lookup-handle sheet b name)
	      when found collect found)))
    (if (null (cdr found))
	(car found)
	(error "handled defined more than once, ~s: ~s" name found))))

(defmethod parse-book ((b ido-pathway-book) &key (handles? t))
  (when handles? 
    (loop for sheet in (parsed-sheets b) do (clear-handles sheet)))
  (loop for block-type in (append (if handles? '(parsed-handle-block) nil)
				  '(parsed-sheet-info-block parsed-complex-block parsed-process-block))
     do
     (loop for block in (parsed-blocks b)
	when (typep block block-type)
	do (parse-block block))))

(defmethod block-types ((book ido-pathway-book))
  (loop for class in (system:class-direct-subclasses (find-class 'ido-pathway-block))
       collect (list* (class-name class) (class-slot-value class 'block-headers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sheets

(defclass ido-pathway-sheet (parsed-sheet)
  ((lookup-table :accessor lookup-table :initarg :lookup-table :initform (make-hash-table :test 'equalp))
   (handle-lookup-sheets :accessor handle-lookup-sheets)
   ))

(defmethod clear-handles ((s ido-pathway-sheet))
  (setf (lookup-table s) (make-hash-table :test 'equalp)))

(defmethod add-handle ((s ido-pathway-sheet) name object)
  (if (gethash name (lookup-table s))
      (let ((error (format nil "Duplicate handle ~s in ~a:~a,~a" name s object (gethash name (lookup-table s)))))
	(push error (parse-errors object))
	(push error (parse-errors (gethash name (lookup-table s)))))
      (setf (gethash name (lookup-table s)) object)))

(defmethod lookup-handle ((s ido-pathway-sheet) name)
  (or (gethash name (lookup-table s))
      (loop for sheet in (handle-lookup-sheets s)
	 for found = (gethash name (lookup-table sheet))
	 do (return-from lookup-handle found))))

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
   ))

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
   (handle-super :accessor handle-super :initarg :handle-super :initform nil))
  )

(defmethod parse-row ((h parsed-handle))
  (let ((cell-list (cell-list h)))
    (setf (handle h) (first cell-list) (handle-description h) (second cell-list) 
	  (handle-kind h) (third cell-list) (handle-class h) (fourth cell-list)
	  (handle-super h) (fifth cell-list)))
  (add-handle (in-sheet (in-block h)) (handle h) h))

(defmethod print-summary ((o parsed-handle))
  (format nil "~s" (handle o)))

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
  (let ((string (string-trim " " (car (cell-list p)))))
    (flet ((with-stoichiometry (piece)
	     (destructuring-bind (stoichiometry entity) (car (all-matches piece "((\\d+\\s+){0,1})([A-Za-z0-9-]+)" 1 3))
	       (list (if (equal stoichiometry "") 1 (parse-integer stoichiometry )) entity))))
      (cond ((#"matches" string "^(((\\d+\\s+){0,1}[A-Za-z0-9-]+?)\\s*\\+\\s*)*((\\d+\\s+){0,1}[A-Za-z0-9-]+?)\\s*(->)\\s*(((\\d+\\s+){0,1}[A-Za-z0-9-]+?)\\s*\\+\\s*)*((\\d+\\s+){0,1}[A-Za-z0-9-]+?)\\s*$")
	     (let ((parsed
		    (mapcar (lambda(e) (split-at-regex e "\\s*\\+\\s*"))
			    (split-at-regex string "\\s*->\\s*"))))
	       (setf (process-substrates p) (mapcar #'with-stoichiometry (first parsed))
		     (process-products p) (mapcar #'with-stoichiometry (second parsed)))))
	    (t (setf (parse-errors p) (list "didn't match expected form")))))))

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
	 (handles (apply 'append (mapcar 'block-rows (remove 'parsed-handle-block found :key 'type-of :test-not 'eq))))
	 (processes (apply 'append (mapcar 'block-rows (remove 'parsed-process-block found :key 'type-of :test-not 'eq))))
	 (complexes (apply 'append (mapcar 'block-rows (remove 'parsed-complex-block found :key 'type-of :test-not 'eq)))))
    (print-db processes)
    (print-db complexes)
    (check-processes-use-only-defined-handles complexes handles processes)
    book))

(defun report-some-processes (book)
  (loop for bl in (remove 'parsed-process-block (parsed-blocks book) :key 'type-of :test-not 'eq)
     do 
     (loop for p in (parsed-rows bl)
	when (not (parse-errors p)) do (format t "~{~a~^ + ~} -> ~{~a~^ + ~}~%"
					       (loop for p in (process-substrates p)
						  collect (if (equal (car p) 1) (second p) (format nil "~a ~a" (car p) (second p))))
					       (loop for p in (process-products p)
						  collect (if (equal (car p) 1) (second p) (format nil "~a ~a" (car p) (second p))))))))
