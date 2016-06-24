;; this is mostly one-off utility stuff

;; read in-file, write out all s p o where p is the property to a tab delimited spreadsheet.
(defun extract-property (property in-file out-file ontology-url &key debug )
  (let* ((kb (load-kb-jena in-file))
	(labels (rdfs-labels kb)))
    (let ((in-model (#"createDefaultModel" 'modelfactory)))
      (#"read" in-model
	       (new 'bufferedinputstream
		    (#"getInputStream" (#"openConnection" (new 'java.net.url (format nil "file://~a" (namestring (truename in-file)))))))
	       ontology-url)
      (with-open-file (f out-file :direction :output :if-exists :supersede :if-does-not-exist :create)
	(loop with iterator = (#"listStatements" in-model)
	   while (#"hasNext" iterator)
	   for statement = (#"next" iterator)
	   for subject = (#"getSubject" statement)
	   for object = (#"getObject" statement)
	   for predicate = (#"getPredicate" statement)
	   for replace = nil
	   for replacement = nil
	   for count from 0
	   do
	     (when (equal (#"toString" (#"getURI" predicate)) property)
	   (format f "~a	~a	~a	~a~%"
		   count (gethash (make-uri (#"toString" (#"getURI" subject))) labels)
		   (#"replaceAll" (#"toString" (#"getURI" subject)) ".*/" "")
		   (#"replaceAll" (#"toString" (#"getString" (setq @ object))) "\\n" "\\\\n"))))))))

;; excel munges double quotes, deciding to add them when serializing
;; text files, and using "" to quote them.  This attempts to undo
;; this. Unfortunately id doesn't completely work. If a field starts
;; with a " but doesn't end with one, then excel changes things in a
;; way that can't be recovered.

(defun unexcel-quotes (string)
  (if (or (null string) (equal string "") (#"matches" string "^\\s+$"))
      string
      (progn 
	(when (and (char= (char string 0) #\") (char= (char string (1- (length string))) #\"))
	  (setq string (subseq string 1 (- (length string) 1))))
	(#"replaceAll" string "\"\"" "\""))))

;; take two files in and merge them based on the value of second, third, and fourth row (after account for excel quotation issues)
(defun merge-tab-delimited (first second out)
  (let ((ft (make-hash-table :test 'equalp)))
    (with-open-file (f first)
      (with-open-file (s second)
	(with-open-file (o out :direction :output :if-does-not-exist :create :if-exists :supersede)
	  (loop for line = (read-line f nil :eof)
	     until (eq line :eof)
	     for row = (split-at-char line #\tab)
	     do
	       (setf (gethash (list (unexcel-quotes (second row)) (third row) (unexcel-quotes (fourth row))) ft)
		      row)
	       (write-line line o))
	  (setq @@ ft)
	  (loop for line = (read-line s nil :eof)
	     until (eq line :eof)
	     for row = (split-at-char line #\tab)
	     unless (gethash (list (second row) (third row) (fourth row)) ft)
	     do (write-line line o) ))))))

;; reads in a spreadsheet saying what to do about properties. 
;; property is the property in question
;; in-ontology is the obi.owl file
;; in-edits is the spreadsheet saying what should happen
;; out-ontology-1 is in-ontology minus all property assertions (of property)
;; out-ontology-2 is property assertions (of property) that was want to keep in the main file (editor notes)
;; out-ontology-3 is property assertions (of property) that was want to keep in a separate file (curation notes)
;; ontology-url is the xml:base
;; 
;; If you ever want to use this, you probably want to modify the choice criteria which is hard coded
;; -> (member disposition '("separate" "check" "todo" "asked jennifer") :test 'equal)

(defun act-on-property-edits (property in-ontology in-edits out-ontology-1 out-ontology-2 out-ontology-3 ontology-url &key debug )
  (let* ((kb (load-kb-jena in-ontology))
	 (labels (rdfs-labels kb))
	 (ft (make-hash-table :test 'equalp))
	 (ont-model (create-empty-obi-model))
	 (keep-props-model (create-empty-obi-model))
	 (separate-props-model (create-empty-obi-model)))
    (with-open-file (f in-edits)
      (loop for line = (read-line f nil :eof)
	 until (eq line :eof)
	 for row = (split-at-char line #\tab)
	 do
	 (setf (gethash (list (unexcel-quotes (second row)) (third row) (unexcel-quotes (fourth row))) ft) row)
	 )
      (let ((in-model (#"createDefaultModel" 'modelfactory)))
	(#"read" in-model
		 (new 'bufferedinputstream
		      (#"getInputStream" (#"openConnection" (new 'java.net.url (format nil "file://~a" (namestring (truename in-ontology)))))))
		 ontology-url)
	(loop with iterator = (#"listStatements" in-model)
	   while (#"hasNext" iterator)
	   for statement = (#"next" iterator)
	   for subject = (#"getSubject" statement)
	   for object = (#"getObject" statement)
	   for predicate = (#"getPredicate" statement)
	   for count from 0

	   do
	   (if (equal (#"toString" (#"getURI" predicate)) property)
	     (let* ((id (#"replaceAll" (#"toString" (#"getURI" subject)) ".*/" ""))
		    (label (gethash (make-uri (#"toString" (#"getURI" subject))) labels))
		    (note (#"replaceAll" (#"toString" (#"getString" (setq @ object))) "\\n" "\\\\n"))
		    (disposition  (let ((it (sixth (gethash (list label id note) ft))))
				    (if (or (not it)  (equal it ""))
					(fifth (gethash (list label id note) ft))
					it))))
;	       (print-db (gethash (list label id note) ft))
	       (cond 
		 ((equal disposition "user") (#"add" keep-props-model statement))
		 ((member disposition '("separate" "check" "todo" "asked jennifer") :test 'equal)
		  (#"add" separate-props-model statement))
		 ((null disposition)
		  (format t "~a: ~a~%" disposition (list label id (subseq note 0 (min 20 (length note))))))
		 (t '(format t "~a: ~a~%" disposition (list label id (subseq note 0 (min 20 (length note))))))
		 ))
	     (#"add" ont-model statement)
	     ))
	(write-jena-model ont-model out-ontology-1)
	(write-jena-model keep-props-model out-ontology-2)
	(write-jena-model separate-props-model out-ontology-3)
	))))

